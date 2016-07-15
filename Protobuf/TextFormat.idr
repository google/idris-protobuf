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

module Protobuf.TextFormat

import Data.String
import Control.Monad.State

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Protobuf.Core
import Protobuf.ParseUtils

||| A state monad that stores the string so far, and the current indent.
Printer : Type -> Type
Printer = State (Nat, String)

print : String -> Printer ()
print s = do {
  (indent, buffer) <- get
  put (indent, buffer ++ s)
}

getIndent : Printer Nat
getIndent = do {
  (indent, buffer) <- get
  return indent
}

putIndent : Nat -> Printer ()
putIndent indent' = do {
  (indent, buffer) <- get
  put (indent', buffer)
}

||| Print the current indent.  Typically called at the start of a new line.
printIndent : Printer ()
printIndent = getIndent >>= (print . makeIndent) where
  makeIndent : Nat -> String
  makeIndent Z     = ""
  makeIndent (S n) = "  " ++ (makeIndent n)

||| Prints the expression in braces
inBraces : Printer () -> Printer ()
inBraces p = do {
  print "{\n"
  indent <- getIndent
  putIndent (indent + 1)
  p
  putIndent indent
  printIndent
  print "}"
}

printEnum : interpEnum d -> Printer ()
printEnum {d=MkEnumDescriptor {k=k} _ values} i = print (name (index i values))

||| Does an action for each element of a list.
forEach : Monad m => (a -> m ()) -> List a -> m ()
forEach _ Nil       = return ()
forEach f (x :: xs) = do {
  f x
  forEach f xs
}

mutual
  printMessage : InterpMessage d -> Printer ()
  printMessage {d=MkMessageDescriptor _ _} (MkMessage fields) =
    printFields fields

  printFields : InterpFields d -> Printer ()
  printFields {d=Nil} Nil = return ()
  printFields {d=f::fs} (x::xs) = do {
    printField x
    printFields xs
  }

  printField : interpField d -> Printer ()
  printField {d=MkFieldDescriptor Optional _ name number} =
    maybe (return ()) (printSingleFieldValue name)
  printField {d=MkFieldDescriptor Required _ name number} =
    (printSingleFieldValue name)
  printField {d=MkFieldDescriptor Repeated _ name number} =
    forEach (printSingleFieldValue name)

  ||| Prints in the form
  ||| <indent>field_name: <contents>\n
  printSingleFieldValue : (name: String) -> interpFieldValue d -> Printer ()
  printSingleFieldValue name x = do {
    printIndent
    print name
    print ": "
    printFieldValue x
    print "\n"
  }

  printFieldValue : interpFieldValue d -> Printer ()
  printFieldValue {d=PBDouble} = print . show
  printFieldValue {d=PBFloat} = print . show
  printFieldValue {d=PBInt32} = print . show
  printFieldValue {d=PBInt64} = print . show
  printFieldValue {d=PBUInt32} = print . show
  printFieldValue {d=PBUInt64} = print . show
  printFieldValue {d=PBSInt32} = print . show
  printFieldValue {d=PBSInt64} = print . show
  printFieldValue {d=PBFixed32} = print . show
  printFieldValue {d=PBFixed64} = print . show
  printFieldValue {d=PBSFixed32} = print . show
  printFieldValue {d=PBSFixed64} = print . show
  printFieldValue {d=PBBool} = print . toLower . show
  printFieldValue {d=PBString} = print . show
  printFieldValue {d=PBBytes} = print . show
  printFieldValue {d=PBMessage _} = inBraces . printMessage
  printFieldValue {d=PBEnum _} = printEnum

export printToTextFormat : InterpMessage d -> String
printToTextFormat x = assert_total $ snd (execState (printMessage x) (Z, ""))

export implementation Show (InterpMessage d) where
  show = printToTextFormat

--- Deserialization

--- Deserialization is implemented using the Lightyear monadic parser package.
--- TODO: commit to more paths to give a better error stack and faster parsing.

-- TODO: handle escape codes including \"
parseString : Parser String
parseString = do {
  char '"'
  chars <- many (satisfy (\c => c /= '"'))
  char '"'
  spaces
  return (pack chars)
}

parseEnum : Parser (interpEnum d)
parseEnum {d=MkEnumDescriptor enumName values} = do {
  chars <- many (satisfy isAlpha)
  spaces
  case findIndex (\v => name v == pack chars) values of
    Nothing => fail (
      "An field in the enum " ++ enumName ++ " (no field named " ++
      (show (pack chars)) ++ ")")
    Just i => return i
}

mutual
  parseMessage : Parser (InterpMessage d)
  parseMessage {d=MkMessageDescriptor msgName fields} = do {
    xs <- parseFields msgName
    case messageFromFieldList {fields=fields} xs of
      Left err => fail ("A valid message (" ++ err ++ ")")
      Right fs => return (MkMessage fs)
  }

  parseFields : (msgName : String) -> Parser (FieldList d)
  parseFields {d=d} msgName = many (do {
    chars <- some (satisfy (\c => isDigit c || isAlpha c || c == '_'))
    commitTo (do {
      spaces
      token ":"
      case Data.Vect.findIndex (\v => name v == pack chars) d of
        Nothing => fail (
          "An field in the message " ++ msgName ++ " (no field named " ++
          (show (pack chars)) ++ ")")
        Just i => do {
          v <- parseField
          return (i ** v)
        }
    })
  })

  parseField : Parser (singularTypeForField d)
  parseField {d=MkFieldDescriptor _ ty _ _} = parseFieldValue {d=ty}

  parseFieldValue : Parser (interpFieldValue d)
  parseFieldValue {d=PBDouble} = fail "Not Implemented"
  parseFieldValue {d=PBFloat} = fail "Not Implemented"
  parseFieldValue {d=PBInt32} = do {
    chars <- many (satisfy isDigit)
    spaces
    case parseInteger (pack chars) of
      Nothing => fail "Could not parse Int32"
      Just x => return x
  }
  parseFieldValue {d=PBInt64} = fail "Not Implemented"
  parseFieldValue {d=PBUInt32} = fail "Not Implemented"
  parseFieldValue {d=PBUInt64} = fail "Not Implemented"
  parseFieldValue {d=PBSInt32} = fail "Not Implemented"
  parseFieldValue {d=PBSInt64} = fail "Not Implemented"
  parseFieldValue {d=PBFixed32} = fail "Not Implemented"
  parseFieldValue {d=PBFixed64} = fail "Not Implemented"
  parseFieldValue {d=PBSFixed32} = fail "Not Implemented"
  parseFieldValue {d=PBSFixed64} = fail "Not Implemented"
  parseFieldValue {d=PBBool} = fail "Not Implemented"
  parseFieldValue {d=PBString} = parseString
  parseFieldValue {d=PBBytes} = parseString
  parseFieldValue {d=PBMessage m} = braces parseMessage
  parseFieldValue {d=PBEnum e} = parseEnum

export parseFromTextFormat : String -> Either String (InterpMessage d)
parseFromTextFormat {d=d} = assert_total $ parse (spaces *> parseMessage {d=d})
