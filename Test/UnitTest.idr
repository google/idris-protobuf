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

%access export

public export Test : Type -> Type
Test = Either String

failAssert : String -> Test a
failAssert err = Left err

||| Asserts that a Maybe value is not Nothing, and returns the value.
assertNotNothing : Maybe a -> Test a
assertNotNothing Nothing  = failAssert "assertNotNothing failed: value was Nothing"
assertNotNothing (Just x) = return x

||| Asserts that an Either String a, interpreted as an error monad, does not
||| contain an error.
assertNotError : Either String a -> Test a
assertNotError (Left err)  = failAssert $ "assertNotError failed: value contained the error: " ++ err
assertNotError (Right x) = return x

assertError : Either String a -> String -> Test ()
assertError (Left err') err = if err == err'
  then return ()
  else failAssert ("assertError failed: expected: " ++ err ++ ", got: " ++ err')
assertError (Right _) err = failAssert "assertError failed: expected an error, got a result"

assertEq : (Eq a, Show a) => (given : a) -> (expected : a) -> Test ()
assertEq g e = if g == e
  then return ()
  else failAssert ("assertEq failed: got " ++ (show g) ++ ", expected " ++ (show e))

assertEq' : Eq a => (given : a) -> (expected : a) -> Test ()
assertEq' g e = if g == e
    then return ()
    else failAssert "assertEq Failed"

assertNotNil : List a -> Test (a, List a)
assertNotNil Nil       = failAssert ("assertNotNil failed: list was Nil");
assertNotNil (x :: xs) = return (x, xs)

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

||| Does an action for each element of a list.
forEach : Monad m => (a -> m ()) -> List a -> m ()
forEach _ Nil       = return ()
forEach f (x :: xs) = do {
  f x
  forEach f xs
}

runTests : TestFixture -> IO ()
runTests f = do {
  forEach (runTestCase (name f)) (tests f)
}
