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

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Protobuf.Core
import Protobuf.FileDescriptor
import Protobuf.Lookup

requiredSpace : Parser ()
requiredSpace = space *> spaces *> return ()

label : Parser Label
label = (char 'o' >! string "ptional" *> requiredSpace *> return Optional)
    <|> (string "re" >! string "quired" *> requiredSpace *> return Required)
    <|> (string "re" >! string "peated" *> requiredSpace *> return Repeated)
    <?> "Label"

isIdentifierChar : Char -> Bool
isIdentifierChar c = (isAlpha c) || (isDigit c) || c == '_'

identifier : Parser String
identifier = (pure pack) <*> many (satisfy isIdentifierChar) <* spaces

-- Number of field or enum
-- TODO: implement this
number : Parser Int
number = fail "Not implemented"

nothingToErr : (errMsg : String) -> Maybe a -> Parser a
nothingToErr errMsg = maybe (fail errMsg) return

mutual
  fieldDescriptor : FileDescriptor -> Parser FieldDescriptor
  fieldDescriptor fd = do {
    l <- label
    ty <- fieldValueDescriptor fd
    name <- identifier
    token "="
    number <- number
    token ";"
    return (MkFieldDescriptor l ty name number)
  }

  -- TODO: handle non-Message types.
  fieldValueDescriptor : FileDescriptor -> Parser FieldValueDescriptor
  fieldValueDescriptor fd = do {
    ty <- identifier
    msg <- nothingToErr ("Could not find message " ++ ty) (findByName ty (messages fd))
    return (PBMessage msg)
  }
