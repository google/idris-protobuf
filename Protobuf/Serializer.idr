module Protobuf.Serializer

import Protobuf.Core

%default total

-- Export as public since text and wire format code must be able to construct
-- implementations.

||| The interface for a serializer that serializes protocol buffers to some format.
||| A serializer must be able to emit each primitive type, as well as messages
||| composed of fields, via the `startMessage`, `endMessage`, `startField` and
||| `endMessage` functions.
public export interface Monad m => Serializer (m : Type -> Type) where
  startField : String -> m ()
  endField : m ()

  serializeDouble : Double -> m ()
  serializeFloat : Double -> m ()
  serializeInt32 : Integer -> m ()
  serializeInt64 : Integer -> m ()
  serializeUInt32 : Integer -> m ()
  serializeUInt64 : Integer -> m ()
  serializeSInt32 : Integer -> m ()
  serializeSInt64 : Integer -> m ()
  serializeFixed32 : Integer -> m ()
  serializeFixed64 : Integer -> m ()
  serializeSFixed32 : Integer -> m ()
  serializeSFixed64 : Integer -> m ()
  serializeBool : Bool -> m ()
  serializeString : String -> m ()
  serializeBytes : String -> m ()
  serializeEnumValue : EnumValueDescriptor -> m ()
  startMessage : m ()
  endMessage : m ()

||| Does an action for each element of a list.
forEach : Monad m => (a -> m ()) -> List a -> m ()
forEach _ Nil       = return ()
forEach f (x :: xs) = do {
  f x
  forEach f xs
}

serializeEnum : Serializer m => interpEnum e -> m ()
serializeEnum {e=MkEnumDescriptor values} x = serializeEnumValue (index x values)

-- Note: The totality checker doesn't consider these functions total.  I think
-- that the problem is that every recursive call is structurally decreasing in
-- the variable d, except for serializeField when d has the form
-- MkFieldDescriptor Repeated _ _.  In that case it is structurally
-- non-increasing in `d` and structurally decreasing in some other variable.
mutual
  ||| Serializes a message.
  |||
  ||| @ m The serializer.
  ||| @ d The message descriptor.
  partial serializeMessage' : Serializer m => InterpMessage d -> m ()
  serializeMessage' (MkMessage x) = serializeFields x

  ||| Serializes a heterogenous vector of fields.
  |||
  ||| @ m The serializer.
  ||| @ fields The descriptors of the fields.
  partial serializeFields : Serializer m => InterpFields fields -> m ()
  serializeFields {fields=Nil} Nil = return ()
  serializeFields {fields=f::fs} (x::xs) = do {
    serializeField x
    serializeFields xs
  }

  partial serializeSingularField : Serializer m => (name : String) -> interpFieldValue d -> m ()
  serializeSingularField name x = do {
    startField name
    serializeFieldValue x
    endField
  }

  partial serializeField : Serializer m => interpField d -> m ()
  serializeField {d=MkFieldDescriptor Optional _ name} =
    maybe (return ()) (serializeSingularField name)
  serializeField {d=MkFieldDescriptor Required _ name} =
    serializeSingularField name
  serializeField {d=MkFieldDescriptor Repeated ty name} =
    forEach (serializeSingularField name)

  partial serializeFieldValue : Serializer m => interpFieldValue d -> m ()
  serializeFieldValue {d=PBDouble} x    = serializeDouble x
  serializeFieldValue {d=PBFloat} x     = serializeFloat x
  serializeFieldValue {d=PBInt32} x     = serializeInt32 x
  serializeFieldValue {d=PBInt64} x     = serializeInt64 x
  serializeFieldValue {d=PBUInt32} x    = serializeUInt32 x
  serializeFieldValue {d=PBUInt64} x    = serializeUInt64 x
  serializeFieldValue {d=PBSInt32} x    = serializeSInt32 x
  serializeFieldValue {d=PBSInt64} x    = serializeSInt64 x
  serializeFieldValue {d=PBFixed32} x   = serializeFixed32 x
  serializeFieldValue {d=PBFixed64} x   = serializeFixed64 x
  serializeFieldValue {d=PBSFixed32} x  = serializeSFixed32 x
  serializeFieldValue {d=PBSFixed64} x  = serializeSFixed64 x
  serializeFieldValue {d=PBBool} x      = serializeBool x
  serializeFieldValue {d=PBString} x    = serializeString x
  serializeFieldValue {d=PBBytes} x     = serializeBytes x
  serializeFieldValue {d=PBEnum values} x = serializeEnum x
  serializeFieldValue {d=PBMessage d} x = do {
    startMessage
    serializeMessage' x
    endMessage
  }

export serializeMessage : Serializer m => InterpMessage d -> m ()
serializeMessage = assert_total serializeMessage'
