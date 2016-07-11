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
PhoneNumber = MkMessageDescriptor [
  MkFieldDescriptor Required PBString "number",
  MkFieldDescriptor Optional (PBEnum PhoneType) "type"
]

namespace PhoneNumber
  getNumber : InterpMessage PhoneNumber -> String
  getNumber (MkMessage [number, _]) = number

  getType : InterpMessage PhoneNumber -> Maybe (Fin 3)
  getType (MkMessage [_, type])  = type

public export Person : MessageDescriptor
Person = MkMessageDescriptor [
  MkFieldDescriptor Required PBString "name",
  MkFieldDescriptor Required PBInt32 "id",
  MkFieldDescriptor Optional PBString "email",
  MkFieldDescriptor Repeated (PBMessage PhoneNumber) "phone"
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
