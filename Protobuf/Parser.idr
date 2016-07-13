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

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.Combinators

import Protobuf.Core
import Protobuf.FileDescriptor
import Protobuf.Lookup

requiredSpace : Parser ()
requiredSpace = space *> spaces *> return ()

label : Parser Label
label = (char 'o' >! string "ptional" *> requiredSpace *> return Optional)
    <|> (string "req" >! string "uired" *> requiredSpace *> return Required)
    <|> (string "rep" >! string "eated" *> requiredSpace *> return Repeated)
    <?> "Label"

isIdentifierChar : Char -> Bool
isIdentifierChar c = (isAlpha c) || (isDigit c) || c == '_'

identifier : Parser String
identifier = (pure pack) <*> some (satisfy isIdentifierChar) <* spaces

nothingToErr : (errMsg : String) -> Maybe a -> Parser a
nothingToErr errMsg = maybe (fail errMsg) return

-- Number of field or enum
-- TODO: implement this
nonNegative : Parser Int
nonNegative = do {
  digits <- some (satisfy isDigit)
  nothingToErr "Could not parse digits" (parsePositive (pack digits))
}

listToVect : List a -> (k : Nat ** Vect k a)
listToVect Nil = (Z ** Nil)
listToVect (x::xs) = let (k ** xs') = listToVect xs in
  (S k ** (x :: xs'))



enumValueDescriptor : Parser EnumValueDescriptor
enumValueDescriptor = do {
  name <- identifier
  token "="
  number <- nonNegative
  token ";"
  return (MkEnumValueDescriptor name number)
}

-- Consumes n copies of a thing in braces.  Assumes that the thing in question
-- cannot start with "}" and commits to parsing the thing of "}" is not found.
manyInBraces : Parser a -> Parser (List a)
manyInBraces p = (token "{") *> manyInBraces' [] where
  manyInBraces' : List a -> Parser (List a)
  manyInBraces' xs = (token "}" *> return xs) <|> (do {x <- p; manyInBraces' (xs ++ [x])})

enumDescriptor : Parser EnumDescriptor
enumDescriptor = (token "enum") *!> (do {
  name <- identifier
  values <- manyInBraces (enumValueDescriptor)
  (k ** values') <- return (listToVect values)
  return (MkEnumDescriptor name values')
})

mutual
  messageDescriptor : FileDescriptor -> Parser MessageDescriptor
  messageDescriptor fd = (token "message") *!> (do {
    name <- identifier
    fields <- manyInBraces (fieldDescriptor fd)
    (k ** fields') <- return (listToVect fields)
    return (MkMessageDescriptor name fields')
  })

  fieldDescriptor : FileDescriptor -> Parser FieldDescriptor
  fieldDescriptor fd = do {
    l <- label
    ty <- fieldValueDescriptor fd
    name <- identifier
    token "="
    number <- nonNegative
    token ";"
    return (MkFieldDescriptor l ty name number)
  }

  fieldValueDescriptor : FileDescriptor -> Parser FieldValueDescriptor
  fieldValueDescriptor fd = (token "double" *!> return PBDouble)
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
                        <|> (messageType fd)
                        <|> (enumType fd)
                        <?> "The name of a message, enum or primitive type"

  messageType : FileDescriptor -> Parser FieldValueDescriptor
  messageType fd = do {
    ty <- identifier
    msg <- nothingToErr ("Could not find message " ++ ty) (findByName ty (messages fd))
    return (PBMessage msg)
  }


  enumType : FileDescriptor -> Parser FieldValueDescriptor
  enumType fd = do {
    ty <- identifier
    msg <- nothingToErr ("Could not find enum " ++ ty) (findByName ty (enums fd))
    return (PBEnum msg)
  }

addMessageDescriptor : FileDescriptor -> Parser FileDescriptor
addMessageDescriptor (MkFileDescriptor ms es) = do {
  m <- messageDescriptor (MkFileDescriptor ms es)
  return (MkFileDescriptor (ms ++ [m]) es)
}

addEnumDescriptor : FileDescriptor -> Parser FileDescriptor
addEnumDescriptor (MkFileDescriptor ms es) = do {
  e <- enumDescriptor
  return (MkFileDescriptor ms (es ++ [e]))
}

fileDescriptor : FileDescriptor -> Parser FileDescriptor
fileDescriptor fd = (eof *> return fd) <|> (do {
  fd' <- (addMessageDescriptor fd) <|> (addEnumDescriptor fd)
  fileDescriptor fd'
})

export parseFileDescriptor : String -> Either String FileDescriptor
parseFileDescriptor = parse (spaces *> fileDescriptor (MkFileDescriptor [] []))
