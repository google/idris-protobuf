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

module Test.Parser

import Test.Utils
import Protobuf.Core
import Protobuf.FileDescriptor
import Protobuf.Parser
import Protobuf.Eq

%access export

protoFileContents : String
protoFileContents = unlines [
  "enum PhoneType {MOBILE = 0; HOME = 1; WORK = 2;}",
  "message PhoneNumber {",
  "  required string number = 0;",
  "  optional PhoneType type = 1;",
  "}",
  "message Person {",
  "  required string name = 0;",
  "  required int32 id = 1;",
  "  optional string email = 2;",
  "  repeated PhoneNumber phone = 3;",
  "}",
  ""
]

expectedFileDescriptor : FileDescriptor
expectedFileDescriptor = MkFileDescriptor
  [PhoneNumber, Person]
  [PhoneType]

testParseFileDescriptor : IO ()
testParseFileDescriptor = assertEq'
  (parseFileDescriptor protoFileContents)
  (Right expectedFileDescriptor)
