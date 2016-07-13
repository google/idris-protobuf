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
||| Utility functions to lookup things by name or number.

module Protobuf.Lookup

import Protobuf.Core

%access export

interface Named a where
  getName : a -> String

interface Numbered a where
  getNumber : a -> Int

findByName : Named a => String -> List a -> Maybe a
findByName name = find (\x => getName x == name)

lookupByNameOrNumber : (Named a, Numbered a) => Either String Int -> Vect k a -> Maybe (Fin k)
lookupByNameOrNumber (Left name) = findIndex (\x => getName x == name)
lookupByNameOrNumber (Right number) = findIndex (\x => getNumber x == number)

implementation Numbered EnumValueDescriptor where
  getNumber = number

implementation Named EnumValueDescriptor where
  getName = name

implementation Numbered FieldDescriptor where
  getNumber = number

implementation Named FieldDescriptor where
  getName = name

implementation Named EnumDescriptor where
  getName = name

implementation Named MessageDescriptor where
  getName = name
