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

module Test.Utils

import Protobuf.Core

%access export

assertEq : (Eq a, Show a) => (given : a) -> (expected : a) -> IO ()
assertEq g e = if g == e
    then putStrLn "Test Passed"
    else putStrLn ("Test Failed, got " ++ (show g) ++ " expected: " ++ (show e))

public export PhoneType : EnumDescriptor
PhoneType = MkEnumDescriptor [
  MkEnumValueDescriptor "MOBILE" 0,
  MkEnumValueDescriptor "HOME" 1,
  MkEnumValueDescriptor "WORK" 5
]

public export PhoneNumber : MessageDescriptor
PhoneNumber = MkMessageDescriptor "PhoneNumber" [
  MkFieldDescriptor Required PBString "number" 0,
  MkFieldDescriptor Optional (PBEnum PhoneType) "type" 1
]

namespace PhoneNumber
  getNumber : InterpMessage PhoneNumber -> String
  getNumber (MkMessage [number, _]) = number

  getType : InterpMessage PhoneNumber -> Maybe (Fin 3)
  getType (MkMessage [_, type])  = type

public export Person : MessageDescriptor
Person = MkMessageDescriptor "Person" [
  MkFieldDescriptor Required PBString "name" 0,
  MkFieldDescriptor Required PBInt32 "id" 1,
  MkFieldDescriptor Optional PBString "email" 2,
  MkFieldDescriptor Repeated (PBMessage PhoneNumber) "phone" 3
]

namespace Person
  getName : (InterpMessage Person) -> String
  getName (MkMessage [name, _, _, _]) = name

  getId : (InterpMessage Person) -> Integer
  getId (MkMessage [_, id, _, _]) = id

  getEmail : (InterpMessage Person) -> Maybe String
  getEmail (MkMessage [_, _, email, _]) = email

  getPhoneNumbers : (InterpMessage Person) -> List (InterpMessage PhoneNumber)
  getPhoneNumbers (MkMessage [_, _, _, phone_number]) = phone_number

John : InterpMessage Person
John = MkMessage [
  "John Doe",
  1234,
  Just "jdoe@example.com",
  [
    MkMessage ["123-456-7890", Just 1],
    MkMessage ["987-654-3210", Nothing]
  ]
]
