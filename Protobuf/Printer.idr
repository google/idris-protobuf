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
||| A monad for printing with indents.  Used for TextFormat and for implementing
||| Show for MessageDescriptor and other descriptors.

module Protobuf.Printer

import public Control.Monad.State

-- This shouldn't need to be public, but it's hard to export just the
-- Monad implementation of Printer and nothing else.
||| A state monad that stores the string so far, and the current indent.
public export Printer : Type -> Type
Printer = State (Nat, String)

export print : String -> Printer ()
print s = do {
  (indent, buffer) <- get
  put (indent, buffer ++ s)
}

getIndent : Printer Nat
getIndent = do {
  (indent, buffer) <- get
  return indent
}

putIndent : Nat -> Printer ()
putIndent indent' = do {
  (indent, buffer) <- get
  put (indent', buffer)
}

||| Print the current indent.  Typically called at the start of a new line.
export printIndent : Printer ()
printIndent = getIndent >>= (print . makeIndent) where
  makeIndent : Nat -> String
  makeIndent Z     = ""
  makeIndent (S n) = "  " ++ (makeIndent n)

||| Prints the expression in braces
export braces : Printer () -> Printer ()
braces p = do {
  print "{\n"
  indent <- getIndent
  putIndent (indent + 1)
  p
  putIndent indent
  printIndent
  print "}"
}

export runPrinter : Printer () -> String
runPrinter p = snd $ execState p (Z, "")
