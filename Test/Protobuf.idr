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
