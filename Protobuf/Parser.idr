||| Copyright 2016 Google Inc.
|||
||| Licensed under the Apache License, Version 2.0 (the "License");
||| you may not use this file except in compliance with the License.
||| You may obtain a copy of the License at
|||
|||     http://www.apache.org/licenses/LICENSE-2.0
|||
||| Unless required by applicable law or agreed to in writing, software
||| distributed under the License is distributed on an "AS IS" BASIS,
||| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
||| See the License for the specific language governing permissions and
||| limitations under the License.
|||
||| This module parses protocol descriptor files and parses them into protocol
||| buffer descriptors (TODO: create a file descriptor that contains multiple
||| messages).
|||
||| This module can be used together with Idris' type provider language
||| extension, to parse protocol message descriptions from files at compile
||| time and construct types based on them.

module Protobuf.Parser

import Data.String
import Control.Monad.State

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.Combinators
import Lightyear.Position

import Protobuf.Core

record ParserState where
  constructor MkParserState
  scope : String
  messages : List MessageDescriptor
  enums : List EnumDescriptor

ProtoParser : Type -> Type
ProtoParser = ParserT String (State ParserState)

addMessage : MessageDescriptor -> ProtoParser ()
addMessage msg = do {
  (MkParserState scope messages enums) <- get
  put (MkParserState scope (messages ++ [msg]) enums)
}

addEnum : EnumDescriptor -> ProtoParser ()
addEnum enum = do {
  (MkParserState scope messages enums) <- get
  put (MkParserState scope messages (enums ++ [enum]))
}

||| Pushes a name to the scope and returns the original scope.
pushScope : String -> ProtoParser String
pushScope name = do {
  (MkParserState scope messages enums) <- get
  put (MkParserState (scope ++ name ++ ".") messages enums)
  pure scope
}

getScope : ProtoParser String
getScope = do {
  (MkParserState scope messages enums) <- get
  pure scope
}

setScope : String -> ProtoParser ()
setScope scope = do {
  (MkParserState _ messages enums) <- get
  put (MkParserState scope messages enums)
}


requiredSpace : ProtoParser ()
requiredSpace = space *> spaces *> pure ()

label : ProtoParser Label
label = (char 'o' >! string "ptional" *> requiredSpace *> pure Optional)
    <|> (string "req" >! string "uired" *> requiredSpace *> pure Required)
    <|> (string "rep" >! string "eated" *> requiredSpace *> pure Repeated)
    <?> "Label"

isIdentifierChar : Char -> Bool
isIdentifierChar c = (isAlpha c) || (isDigit c) || c == '_' || c == '.'

identifier : ProtoParser String
identifier = (map pack (some (satisfy isIdentifierChar))) <* spaces

nothingToErr : (errMsg : String) -> Maybe a -> ProtoParser a
nothingToErr errMsg = maybe (fail errMsg) pure

nonNegative : ProtoParser Int
nonNegative = do {
  digits <- some (satisfy isDigit)
  nothingToErr "Could not parse digits" (parsePositive (pack digits))
}

listToVect : List a -> (k : Nat ** Vect k a)
listToVect Nil = (Z ** Nil)
listToVect (x::xs) = let (k ** xs') = listToVect xs in
  (S k ** (x :: xs'))

enumValueDescriptor : ProtoParser EnumValueDescriptor
enumValueDescriptor = do {
  name <- identifier
  token "="
  number <- nonNegative
  token ";"
  pure (MkEnumValueDescriptor name number)
}

enumDescriptor : ProtoParser ()
enumDescriptor = (token "enum") *!> (do {
  name <- identifier
  values <- braces (many (enumValueDescriptor))
  (k ** values') <- pure (listToVect values)
  scope <- getScope
  addEnum (MkEnumDescriptor (scope ++ name) values')
})

messageType : ProtoParser FieldValueDescriptor
messageType = do {
  msgName <- identifier
  state <- get
  case (find (\x => name x == msgName) (messages state)) of
    Nothing => fail $ "A message type (no message named " ++ msgName ++ ")"
    Just msg => pure (PBMessage msg)
}

enumType : ProtoParser FieldValueDescriptor
enumType = do {
  enumName <- identifier
  state <- get
  case (find (\x => name x == enumName) (enums state)) of
    Nothing => fail $ "An enum type (no enum named " ++ enumName ++ ")"
    Just enum => pure (PBEnum enum)
}

fieldValueDescriptor : ProtoParser FieldValueDescriptor
fieldValueDescriptor = (token "double" *!> pure PBDouble)
                    <|> (token "float" *!> pure PBFloat)
                    <|> (token "int32" *!> pure PBInt32)
                    <|> (token "int64" *!> pure PBInt64)
                    <|> (token "uint32" *!> pure PBUInt32)
                    <|> (token "uint64" *!> pure PBUInt64)
                    <|> (token "sint32" *!> pure PBSInt32)
                    <|> (token "sint64" *!> pure PBSInt64)
                    <|> (token "fixed32" *!> pure PBFixed32)
                    <|> (token "fixed64" *!> pure PBFixed64)
                    <|> (token "sfixed32" *!> pure PBSFixed32)
                    <|> (token "sfixed64" *!> pure PBSFixed64)
                    <|> (token "bool" *!> pure PBBool)
                    <|> (token "string" *!> pure PBString)
                    <|> (token "bytes" *!> pure PBBytes)
                    <|> messageType
                    <|> enumType
                    <?> "The name of a message, enum or primitive type"

fieldDescriptor : ProtoParser FieldDescriptor
fieldDescriptor = do {
  l <- label
  ty <- fieldValueDescriptor
  name <- identifier
  token "="
  number <- nonNegative
  token ";"
  pure (MkFieldDescriptor l ty name number)
}

messageDescriptor : ProtoParser ()
messageDescriptor = (token "message") *!> (do {
  name <- identifier
  scope <- pushScope name
  -- Parse fields and nested enum and message descriptors
  fields <- braces (many ((fieldDescriptor >>= pure . Just)
                      <|> (messageDescriptor *> pure Nothing)
                      <|> (enumDescriptor *> pure Nothing)))
  setScope scope
  (k ** fields') <- pure (listToVect (mapMaybe (\x => x) fields))
  addMessage (MkMessageDescriptor (scope ++ name) fields')
})

fileDescriptor : ProtoParser ()
fileDescriptor = (many (messageDescriptor <|> enumDescriptor)) *> eof

runParser : String -> Either String ParserState
runParser input =
    let st = execParserT (spaces *> fileDescriptor *> get) (initialState Nothing input 8) in
        case runStateT st $ MkParserState "" [] [] of
            Id (MkReply (ST str _ _) (Failure _), _) => Left str
            Id (MkReply _ (Success _), es) => Right es

export parseFile : String -> Either String (List MessageDescriptor, List EnumDescriptor)
parseFile input = case runParser input of
  Left err => Left err
  Right (MkParserState scope messages enums) => Right (messages, enums)
