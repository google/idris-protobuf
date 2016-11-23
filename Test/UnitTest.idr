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
||| A small unit testing framework.

module Test.UnitTest

import Protobuf.Util

%access export

public export Test : Type -> Type
Test = Either String

failAssert : String -> Test a
failAssert err = Left err

assertEq : (Eq a, Show a) => (given : a) -> (expected : a) -> Test ()
assertEq g e = if g == e
  then pure ()
  else failAssert ("assertEq failed: got " ++ (show g) ++ ", expected " ++ (show e))

assertEq' : Eq a => (given : a) -> (expected : a) -> Test ()
assertEq' g e = if g == e
    then pure ()
    else failAssert "assertEq Failed"

record TestCase where
  constructor MkTestCase
  name : String
  test : Test ()

runTestCase : String -> TestCase -> IO ()
runTestCase f c = let fullname = f ++ "." ++ (name c) in do {
    putStrLn $ "[ RUN      ] " ++ fullname
    case (test c) of
      Left err => do {
        putStrLn err
        putStrLn $ "[   FAILED ] " ++ fullname
      }
      Right () => putStrLn $ "[       OK ] " ++ fullname
}

record TestFixture where
  constructor MkTestFixture
  name : String
  tests : List TestCase

runTests : TestFixture -> IO ()
runTests f = do {
  forEach (runTestCase (name f)) (tests f)
}
