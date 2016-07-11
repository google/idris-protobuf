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
||| This module defines a generalized algebraic data type, `MessageDescriptor`,
||| which is the strongly typed equivalent of the `Descriptor` protocol buffer.
||| Because types are first class values in Idris, we can construct a function
||| `interpMessage` that maps a `MessageDescriptor` to the *type* of structure
||| that the message describes.  The ability to treat types as first class
||| values, and to create generic functions allows us to do in Idris code much
||| of what is done with generated code for protocol buffer implementations in
||| other languages.
module Test.Protobuf

import Test.Utils
import Protobuf.Core

%access export

testGetName : IO ()
testGetName = assertEq (getName John) "John Doe"

testGetId : IO ()
testGetId = assertEq (getId John) 1234

testGetEmail : IO ()
testGetEmail = assertEq (getEmail John) (Just "jdoe@example.com")
