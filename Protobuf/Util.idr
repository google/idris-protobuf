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
||| Equality and Show for descriptors.  Equality for values isn't working so
||| far.

module Protobuf.Util

import Protobuf.Core
import Protobuf.Printer

%access export
%default total

||| Does an action for each element of a list.
forEach : Monad m => (a -> m ()) -> List a -> m ()
forEach _ Nil       = pure ()
forEach f (x :: xs) = do {
  f x
  forEach f xs
}

implementation Eq Label where
  Optional == Optional = True
  Required == Required = True
  Repeated == Repeated = True
  _ == _ = False

implementation Eq EnumValueDescriptor where
  (MkEnumValueDescriptor name value) == (MkEnumValueDescriptor name' value') =
    name == name' && value == value'
  _ == _ = False

heteroEq : Eq a => Vect k a -> Vect k' a -> Bool
heteroEq (x::xs) (x'::xs') = x == x'
heteroEq Nil Nil = True
heteroEq _ _ = False

implementation Eq EnumDescriptor where
  (MkEnumDescriptor name values) == (MkEnumDescriptor name' values') =
    name == name' && heteroEq values values'
  _ == _ = False


mutual
  implementation Eq MessageDescriptor where
    (MkMessageDescriptor name fields) == (MkMessageDescriptor name' fields') =
      assert_total $ name == name' && heteroEq fields fields'

  implementation Eq FieldDescriptor where
    (MkFieldDescriptor label ty name number) == (MkFieldDescriptor label' ty' name' number') =
      label == label' && ty == ty' && name == name' && number == number'

  implementation Eq FieldValueDescriptor where
    PBDouble == PBDouble = True
    PBFloat == PBFloat = True
    PBInt32 == PBInt32 = True
    PBInt64 == PBInt64 = True
    PBUInt32 == PBUInt32 = True
    PBUInt64 == PBUInt64 = True
    PBSInt32 == PBSInt32 = True
    PBSInt64 == PBSInt64 = True
    PBFixed32 == PBFixed32 = True
    PBFixed64 == PBFixed64 = True
    PBSFixed32 == PBSFixed32 = True
    PBSFixed64 == PBSFixed64 = True
    PBBool == PBBool = True
    PBString == PBString = True
    PBBytes == PBBytes = True
    (PBMessage m) == (PBMessage m') = m == m'
    (PBEnum e) == (PBEnum e') = e == e'
    _ == _ = False


implementation Show Label where
  show Optional = "optional"
  show Required = "required"
  show Repeated = "repeated"

printEnumValueDescriptor : EnumValueDescriptor -> Printer ()
printEnumValueDescriptor (MkEnumValueDescriptor name value) =
  printIndent *> print (name ++ " = " ++ (show value) ++ ";\n")

printEnumDescriptor : EnumDescriptor -> Printer ()
printEnumDescriptor (MkEnumDescriptor name values) =
    print ("enum " ++ name  ++ " ") *> braces (forEach printEnumValueDescriptor (toList values))

implementation Show EnumDescriptor where
  show = runPrinter . printEnumDescriptor

implementation Show FieldValueDescriptor where
  show PBDouble = "double"
  show PBFloat = "float"
  show PBInt32 = "int32"
  show PBInt64 = "int64"
  show PBSInt32 = "sint32"
  show PBSInt64 = "sint64"
  show PBUInt32 = "uint32"
  show PBUInt64 = "uint64"
  show PBFixed32 = "fixed32"
  show PBFixed64 = "fixed64"
  show PBSFixed32 = "sfixed32"
  show PBSFixed64 = "sfixed64"
  show PBBool = "bool"
  show PBString = "string"
  show PBBytes = "bytes"
  show (PBMessage m) = name m
  show (PBEnum e) = name e

printFieldDescriptor : FieldDescriptor -> Printer ()
printFieldDescriptor (MkFieldDescriptor label ty name number) =
  printIndent *> print (
    (show label) ++ " " ++ (show ty) ++ " " ++ name ++ " = " ++ (show number) ++ ";\n")

printMessageDescriptor : MessageDescriptor -> Printer ()
printMessageDescriptor (MkMessageDescriptor name fields) =
    print ("message " ++ name ++ " ") *> braces (forEach printFieldDescriptor (toList fields))

implementation Show MessageDescriptor where
  show = runPrinter . printMessageDescriptor
