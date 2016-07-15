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
import Protobuf.Deserializer

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
  print "{"
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

export serializeToTextFormat : InterpMessage d -> String
serializeToTextFormat x = assert_total $ snd (execState (printMessage x) (Z, ""))

export implementation Show (InterpMessage d) where
  show = serializeToTextFormat

--- Deserialization

--- Deserialization is implemented using the Lightyear monadic parser package.
--- TODO: commit to paths to give a better error stack.

TextDeserializer : Type -> Type
TextDeserializer = Parser

implementation Deserializer TextDeserializer where
  deserializeDouble = fail "Not Implemented"
  deserializeFloat = fail "Not Implemented"
  deserializeInt32 = assert_total $ do {
    chars <- many (satisfy isDigit)
    spaces
    case parseInteger (pack chars) of
      Nothing => fail "Could not parse Int32"
      Just x => return x
  }
  deserializeInt64 = fail "Not Implemented"
  deserializeUInt32 = fail "Not Implemented"
  deserializeUInt64 = fail "Not Implemented"
  deserializeSInt32 = fail "Not Implemented"
  deserializeSInt64 = fail "Not Implemented"
  deserializeFixed32 = fail "Not Implemented"
  deserializeFixed64 = fail "Not Implemented"
  deserializeSFixed32 = fail "Not Implemented"
  deserializeSFixed64 = fail "Not Implemented"
  deserializeBool = fail "Not Implemented"
  -- TODO: handle escape codes including \"
  deserializeString = assert_total $ do {
    char '"'
    chars <- many (satisfy (\c => c /= '"'))
    char '"'
    spaces
    return (pack chars)
  }
  deserializeBytes = deserializeString

  startMessage = assert_total $ token "{"
  isEndMessage =
    assert_total $ ((eof <|> token "}") *> return True) <|> (return False)

  readFieldNameOrNumber = assert_total $ do {
    chars <- many (satisfy (\c => c /= ':' && not (isSpace c)))
    spaces
    token ":"
    return (Left (pack chars))
  }

  readEnumValueNameOrNumber = assert_total $ do {
    chars <- many (satisfy isAlpha)
    spaces
    return (Left (pack chars))
  }

  error e = fail (show e)

export deserializeFromTextFormat : String -> Either String (InterpMessage d)
deserializeFromTextFormat {d=d} str =
  parse (assert_total $ spaces *> deserializeMessage {m=TextDeserializer} {d=d}) str
