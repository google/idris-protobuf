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
|||
||| We try to follow the naming conventions for protocol buffer descriptors
||| found in [descriptor.proto](https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.descriptor.pb) with the exception that we use the name
||| `MessageDescriptor` instead of `Descriptor`.

module Protobuf.Core

import public Data.Fin
import public Data.Vect

%default total
%access public export

||| A `Label` is an annotation on a field that describes what kind of data type
||| the field represents.
data Label : Type where
  ||| `Optional` fields are represented as a `Maybe` type.
  Optional : Label
  ||| `Required` fields are represented as a single value.
  Required : Label
  ||| `Repeated` fields are represented as a `List`.
  Repeated : Label

||| A descriptor for a single value in an enum.
data EnumValueDescriptor : Type where
  ||| An enum value with given string and numeric value.
  |||
  ||| @name The string value of the enum.
  ||| @number The integer value of the enum.  This is not the same value is the index of the enum in a PBEnum constructor.
  MkEnumValueDescriptor : (name : String) -> (number : Int) -> EnumValueDescriptor

||| A descriptor for an enum.
data EnumDescriptor : Type where
  ||| An enum of fixed size with given values
  |||
  ||| @k The size of the enum
  ||| @values The possible values of the enum.  Note that enum values have a
  ||| numeric value that is possibly different from the index of the value in
  ||| `values`, e.g. `values` could be `[MkEnumValueDescriptor "five" 5]`.
  MkEnumDescriptor : (values : Vect k EnumValueDescriptor) -> EnumDescriptor

mutual
  ||| A descriptor that describes the type of a protocol buffer message.  This
  ||| describes the type of a protocol buffer message, which is a list of
  ||| fields.
  data MessageDescriptor : Type where
    ||| TODO: add comment
    |||
    ||| @fields The fields of the message.
    MkMessageDescriptor : (fields : Vect k FieldDescriptor) -> MessageDescriptor

  ||| A descriptor that describes the type of a protocol buffer field.
  data FieldDescriptor : Type where
    ||| Constructor for a field with given label, type and name of a field.  E.g.
    ||| `MkFieldDescriptor Optional PBDouble "temperature"` describes an
    ||| optional field whose name is "temperature" of type PBDouble, which is
    ||| interpreted Idris as a `Maybe double`.
    |||
    ||| @label Determines whether the field is optional, required or repeated.
    ||| @ty The type of a single value in the field.
    ||| @name The field's name, used in the text format.
    MkFieldDescriptor : (label : Label) -> (ty : FieldValueDescriptor) -> (name : String) -> FieldDescriptor

  ||| This does not correspond to anything in `descriptor.proto`, but is a
  ||| necessary definition here.  It describes the type of the value of a
  ||| protocol buffer required field, i.e. a single value.  This may be a
  ||| message, an enum or a primitive value.
  data FieldValueDescriptor : Type where
    PBDouble : FieldValueDescriptor
    PBFloat : FieldValueDescriptor
    PBInt32 : FieldValueDescriptor
    PBInt64 : FieldValueDescriptor
    PBUInt32 : FieldValueDescriptor
    PBUInt64 : FieldValueDescriptor
    PBSInt32 : FieldValueDescriptor
    PBSInt64 : FieldValueDescriptor
    PBFixed32 : FieldValueDescriptor
    PBFixed64 : FieldValueDescriptor
    PBSFixed32 : FieldValueDescriptor
    PBSFixed64 : FieldValueDescriptor
    PBBool : FieldValueDescriptor
    PBString : FieldValueDescriptor
    PBBytes : FieldValueDescriptor
    ||| A value which is itself a message.
    PBMessage : MessageDescriptor -> FieldValueDescriptor
    PBEnum : EnumDescriptor -> FieldValueDescriptor

interpEnum : EnumDescriptor -> Type
interpEnum (MkEnumDescriptor {k=k} _) = Fin k

-- NOTE: we implement Interp* as inductive types rather than as functions,
-- because these allow us to implement type classes such as Eq and Show.
--
-- E.g. if we had a function interpMessage : MessageDescriptor -> Type
-- it would not be possible to define a typeclass implementation
--
-- implementation Eq (interpMessage d) where ...
--
-- but it is possible to define
--
-- implementation Eq (InterpMessage d) where ...
--
-- Type classes also operate differently with respect to unfolding, though
-- that didn't pose any problems while working with functions.

mutual
  ||| Takes a message descriptor and returns the type that that message
  ||| descriptor describes.  This is implemented with the algebraic data type
  ||| `InterpMessage`.
  data InterpMessage : MessageDescriptor -> Type where
    MkMessage : InterpFields fields -> InterpMessage (MkMessageDescriptor fields)

  ||| `InterpMessage` the algebraic data type used to implement `interpMessage`.
  ||| `InterpMessage fs` is a heterogeneous vector where where the type of each
  ||| element is `interpField` applied to the corresponding element in `fs`.
  ||| While it is possible to construct this type using `Data.HVect`, I haven't
  ||| found a way to satisfy the totality checker when using this approach.
  data InterpFields : (Vect k FieldDescriptor) -> Type where
    Nil  : InterpFields Nil
    (::) : (interpField f) -> (InterpFields m) -> InterpFields (f :: m)

  ||| Takes a field descriptor and returns the type that that field descriptor
  ||| describes.  This is a single value, a `Maybe` or a `List` depending on
  ||| the field's label.
  interpField : FieldDescriptor -> Type
  interpField (MkFieldDescriptor Optional value _) = Maybe (interpFieldValue value)
  interpField (MkFieldDescriptor Required value _) = interpFieldValue value
  interpField (MkFieldDescriptor Repeated value _) = List (interpFieldValue value)

  ||| Interprets a single field value, i.e. the value described by a required
  ||| field.  For primitive types this is a corresponding Idris primitive type.
  ||| For messages this is `interpMessage` for that message.  For enum's it is
  ||| `Fin k` where `k` is the number of values in the enum.
  interpFieldValue : FieldValueDescriptor -> Type
  interpFieldValue PBDouble = Double
  interpFieldValue PBFloat = Double
  interpFieldValue PBInt32 = Integer
  interpFieldValue PBInt64 = Integer
  interpFieldValue PBUInt32 = Integer
  interpFieldValue PBUInt64 = Integer
  interpFieldValue PBSInt32 = Integer
  interpFieldValue PBSInt64 = Integer
  interpFieldValue PBFixed32 = Integer
  interpFieldValue PBFixed64 = Integer
  interpFieldValue PBSFixed32 = Integer
  interpFieldValue PBSFixed64 = Integer
  interpFieldValue PBBool = Bool
  interpFieldValue PBString = String
  interpFieldValue PBBytes = String
  interpFieldValue (PBMessage m) = InterpMessage m
  interpFieldValue (PBEnum e) = interpEnum e
