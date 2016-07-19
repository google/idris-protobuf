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

import Protobuf.Core

record ParserState where
  constructor MkParserState
  scope : List String
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

requiredSpace : ProtoParser ()
requiredSpace = space *> spaces *> return ()

label : ProtoParser Label
label = (char 'o' >! string "ptional" *> requiredSpace *> return Optional)
    <|> (string "req" >! string "uired" *> requiredSpace *> return Required)
    <|> (string "rep" >! string "eated" *> requiredSpace *> return Repeated)
    <?> "Label"

isIdentifierChar : Char -> Bool
isIdentifierChar c = (isAlpha c) || (isDigit c) || c == '_'

identifier : ProtoParser String
identifier = (pure pack) <*> some (satisfy isIdentifierChar) <* spaces

nothingToErr : (errMsg : String) -> Maybe a -> ProtoParser a
nothingToErr errMsg = maybe (fail errMsg) return

-- Number of field or enum
-- TODO: implement this
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
  return (MkEnumValueDescriptor name number)
}

enumDescriptor : ProtoParser ()
enumDescriptor = (token "enum") *!> (do {
  name <- identifier
  values <- braces (many (enumValueDescriptor))
  (k ** values') <- return (listToVect values)
  addEnum (MkEnumDescriptor name values')
})

messageType : ProtoParser FieldValueDescriptor
messageType = do {
  msgName <- identifier
  state <- get
  case (find (\x => name x == msgName) (messages state)) of
    Nothing => fail $ "A message type (no message named " ++ msgName ++ ")"
    Just msg => return (PBMessage msg)
}

enumType : ProtoParser FieldValueDescriptor
enumType = do {
  enumName <- identifier
  state <- get
  case (find (\x => name x == enumName) (enums state)) of
    Nothing => fail $ "An enum type (no enum named " ++ enumName ++ ")"
    Just enum => return (PBEnum enum)
}

fieldValueDescriptor : ProtoParser FieldValueDescriptor
fieldValueDescriptor = (token "double" *!> return PBDouble)
                    <|> (token "float" *!> return PBFloat)
                    <|> (token "int32" *!> return PBInt32)
                    <|> (token "int64" *!> return PBInt64)
                    <|> (token "uint32" *!> return PBUInt32)
                    <|> (token "uint64" *!> return PBUInt64)
                    <|> (token "sint32" *!> return PBSInt32)
                    <|> (token "sint64" *!> return PBSInt64)
                    <|> (token "fixed32" *!> return PBFixed32)
                    <|> (token "fixed64" *!> return PBFixed64)
                    <|> (token "sfixed32" *!> return PBSFixed32)
                    <|> (token "sfixed64" *!> return PBSFixed64)
                    <|> (token "bool" *!> return PBBool)
                    <|> (token "string" *!> return PBString)
                    <|> (token "bytes" *!> return PBBytes)
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
  return (MkFieldDescriptor l ty name number)
}

messageDescriptor : ProtoParser ()
messageDescriptor = (token "message") *!> (do {
  name <- identifier
  fields <- braces (many fieldDescriptor)
  (k ** fields') <- return (listToVect fields)
  addMessage (MkMessageDescriptor name fields')
})

fileDescriptor : ProtoParser ()
fileDescriptor = (many (messageDescriptor <|> enumDescriptor)) *> eof

runParser : String -> Either String ParserState
runParser input = case output of
    Success _ x => Right x
    Failure es  => Left $ formatError input es
  where output : Result String ParserState
        output = evalState
          (execParserT (spaces *> fileDescriptor *> get) input)
          (MkParserState [] [] [])

export parseFile : String -> Either String (List MessageDescriptor, List EnumDescriptor)
parseFile input = case runParser input of
  Left err => Left err
  Right (MkParserState scope messages enums) => Right (messages, enums)
