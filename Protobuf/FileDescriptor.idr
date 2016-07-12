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
||| This module defines the FileDescriptor type.  Currently it just lists all
||| messages and enums defined in the proto.  Currently only top level
||| definitions are supported and future version will probably list all messages
||| in the file regardless of whether they are top level or not.  That is
||| because nested messages carry no extra semantic meaning for us, since
||| generated code does not generate any kind of namespaces, which must be done
||| by the user, since type providers can't generate their own names or
||| namespaces.

module Protobuf.FileDescriptor

import Protobuf.Core

record FileDescriptor where
  constructor MkFileDescriptor
  messages : List MessageDescriptor
  enums : List EnumDescriptor
