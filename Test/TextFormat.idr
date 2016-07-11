module Test.TextFormat

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
  "}",
  ""
]

testSerializeToTextFormat : IO ()
testSerializeToTextFormat = assertEq (serializeToTextFormat John) johnInTextFormat

implementation (Eq a) => Eq (Provider a) where
  (Provide x)  == (Provide y) = x == y
  (Error x) == (Error y) = x == y
  _ ==  _ = False


-- Implementing Eq (InterpMessage d) is proving difficult so for testing we
-- implement it by serializing to text format and comparing.
implementation Eq (InterpMessage d) where
  x == y = (show x) == (show y)

testDeserializeFromTextFormat : IO ()
testDeserializeFromTextFormat = assertEq
  (deserializeFromTextFormat johnInTextFormat) (Right John)

testDeserializeFromTextFormatWithBadField : IO ()
testDeserializeFromTextFormatWithBadField = assertEq
  (deserializeFromTextFormat {d=Person} "not_a_field: 1")
  (Left "at 1:14 expected:\n  There was no field with name \"not_a_field\"")

testDeserializeFromTextFormatWithMissingRequiredField : IO ()
testDeserializeFromTextFormatWithMissingRequiredField = assertEq
  (deserializeFromTextFormat {d=Person} "id: 1234")
  (Left "at 0:0 expected:\n  The required field \"name\" was not set")

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

testDeserializeFromTextFormatWithOverriddenRequiredField : IO ()
testDeserializeFromTextFormatWithOverriddenRequiredField = assertEq
  (deserializeFromTextFormat (johnInTextFormat ++ "name: \"Jane Doe\""))
  (Right Jane)
