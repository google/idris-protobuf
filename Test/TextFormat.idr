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

module Test.TextFormat

import Test.UnitTest
import Test.Utils
import Protobuf.Core
import Protobuf.TextFormat

%access export
%default total

johnInTextFormat : String
johnInTextFormat = unlines [
  "name: \"John Doe\"",
  "id: 1234",
  "email: \"jdoe@example.com\"",
  "phone: {",
  "  number: \"123-456-7890\"",
  "  type: HOME",
  "}",
  "phone: {",
  "  number: \"987-654-3210\"",
  "}"
]

-- Implementing Eq (InterpMessage d) is proving difficult so for testing we
-- implement it by serializing to text format and comparing.
implementation Eq (InterpMessage d) where
  x == y = (show x) == (show y)

Jane : InterpMessage Person
Jane = MkMessage [
  "Jane Doe",
  1234,
  Just "jdoe@example.com",
  [
    MkMessage ["123-456-7890", Just 1],
    MkMessage ["987-654-3210", Nothing]
  ]
]

allTests : IO ()
allTests = runTests (MkTestFixture "TextFormat" [
  MkTestCase "PrintJohn" (assertEq (printToTextFormat John) johnInTextFormat),
  MkTestCase "ParseJohn"
    (assertEq (parseFromTextFormat {d=Person} johnInTextFormat) (Right John)),
  MkTestCase "ParseMessageWithBadField"
    (assertEq
      (parseFromTextFormat {d=Person} "not_a_field: 1")
      (Left "at 1:14 expected:\n  An field in the message Person (no field named \"not_a_field\")\n")),
  MkTestCase "ParseMessageWithMissingRequiredField"
    (assertEq
        (parseFromTextFormat {d=Person} "id: 1234")
        (Left "at 0:0 expected:\n  A valid message (The required field \"name\" was not set.)\n")),
  MkTestCase "ParseMessageWithOverriddenRequiredField"
    (assertEq (parseFromTextFormat (johnInTextFormat ++ "name: \"Jane Doe\"\n")) (Right Jane)),
  MkTestCase "ParseDouble"
    (assertEq
      (parseFromTextFormat {d=MkMessageDescriptor "test" [
        MkFieldDescriptor Required PBDouble "double_value" 0
      ]} "double_value: -12.34")
      (Right (MkMessage [-12.34]))),
  MkTestCase "ParseBool" (do {
    (assertEq
      (parseFromTextFormat {d=MkMessageDescriptor "test" [
        MkFieldDescriptor Required PBBool "bool_value" 0
      ]} "bool_value: true")
      (Right (MkMessage [True])))
  })
])
