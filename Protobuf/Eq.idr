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
||| Equality for descriptors.  Equality for values isn't working so far.

module Protobuf.Eq

import Protobuf.Core
import Protobuf.FileDescriptor

%access export
%default total

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


implementation Eq FileDescriptor where
  (MkFileDescriptor messages enums) == (MkFileDescriptor messages' enums') =
    messages == messages' && enums == enums'
