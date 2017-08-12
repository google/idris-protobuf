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

module Protobuf.ParseUtils

import Protobuf.Core

%default total

public export singularTypeForField : FieldDescriptor -> Type
singularTypeForField (MkFieldDescriptor _ ty _ _) = interpFieldValue ty

-- Because fields can come in any order, parsing a message is done in two
-- phases.  First, we parse all the fields into a list of pairs
-- (i : Fin k ** interpField (index fields i))
-- Second, for each field we scan through this list and fill in that field's
-- value based on the list (or create an error).
public export FieldList : Vect k FieldDescriptor -> Type
FieldList {k=k} fields = List (i : Fin k ** singularTypeForField (index i fields))

||| Takes a `FieldList`, and selects the elements which represent the 0th field
||| and puts these into a list, and also creates a list of the remaining
||| elements, which are mapped to be part of of a new `FieldList` for the
||| remaining fields.
reduceFieldList : FieldList (f :: fs) -> (List (singularTypeForField f), FieldList fs)
reduceFieldList Nil = (Nil, Nil)
reduceFieldList ((FZ ** x) :: xs) = let (ys, zs) = reduceFieldList xs in
  (x :: ys, zs)
reduceFieldList (((FS k) ** x) :: xs) = let (ys, zs) = reduceFieldList xs in
  (ys, (k ** x) :: zs)

optionalFieldFromList : List (interpFieldValue f) -> Maybe (interpFieldValue f)
optionalFieldFromList Nil      = Nothing
optionalFieldFromList (x::Nil) = Just x
optionalFieldFromList (x::xs)  = optionalFieldFromList xs

fieldFromFieldList : List (singularTypeForField d) -> Either String (interpField d)
fieldFromFieldList {d=MkFieldDescriptor Optional _ _ _} xs = Right (last' xs)
fieldFromFieldList {d=MkFieldDescriptor Required _ name _} xs = case (last' xs) of
    Nothing  => Left ("The required field " ++ (show name) ++ " was not set.")
    (Just x) => Right x
fieldFromFieldList {d=MkFieldDescriptor Repeated _ _ _} xs = Right xs

export messageFromFieldList : FieldList fields -> Either String (InterpFields fields)
messageFromFieldList {fields=Nil} _ = pure Nil
messageFromFieldList {fields=f::fs} xs = let (ys, zs) = reduceFieldList xs in
  case fieldFromFieldList ys of
    Left err => Left err
    Right first => case messageFromFieldList zs of
      Left err => Left err
      Right rest => Right (first :: rest)
