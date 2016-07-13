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
||| Compilation from .proto files to message types can by done within Idris
||| thanks to type providers.  Type providers are a mechanism by which
||| computations, including IO operations, can be done at compile time, with the
||| result of the computation being available at runtime.  This module does not
||| enable the TypeProviders extension, it just provides functions that can be
||| used to provide the types/values.

module Protobuf.Compiler

import Protobuf.Core
import Protobuf.FileDescriptor
import Protobuf.Parser
import Protobuf.Lookup

%access export

parseFileDescriptorFromString : String -> IO (Provider FileDescriptor)
parseFileDescriptorFromString s = do {
  case parseFileDescriptor s of
    Left err => return (Error err)
    Right fd => return (Provide fd)
}

parseFile : String -> IO (Provider FileDescriptor)
parseFile path = do {
  result <- readFile path
  case result of
    Left _ => return (Error ("Could not load file " ++ path))
    Right s => case parseFileDescriptor s of
      Left err => return (Error err)
      Right fd => return (Provide fd)
}

-- This is a pure function but type providers expect IO actions.
lookupMessage : String -> FileDescriptor -> IO (Provider MessageDescriptor)
lookupMessage name fd = return (
  case findByName name (messages fd) of
    Nothing => Error ("Could not find message " ++ name)
    Just m => Provide m
)

lookupEnum : String -> FileDescriptor -> IO (Provider EnumDescriptor)
lookupEnum name fd = return (
  case findByName name (enums fd) of
    Nothing => Error ("Could not find enum " ++ name)
    Just m => Provide m
)
