module Protobuf.TextFormat

import Data.String
import Control.Monad.State

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Protobuf
import Protobuf.Deserializer
import Protobuf.Serializer

-- TextPrinter is a state monad that stores the string so far, and the current
-- indent.
TextSerializer : Type -> Type
TextSerializer = State (Nat, String)

print : String -> TextSerializer ()
print s = do {
  (indent, buffer) <- get
  put (indent, buffer ++ s)
}

makeIndent : Nat -> String
makeIndent Z     = ""
makeIndent (S n) = "  " ++ (makeIndent n)

showBool : Bool -> String
showBool False = "false"
showBool True  = "true"

implementation Serializer TextSerializer where
  startField name                              = do {
    (indent, buffer) <- get
    put (indent, buffer ++ (makeIndent indent) ++ name ++ ": ")
  }
  endField = print "\n"

  serializeDouble x                            = print (show x)
  serializeFloat x                             = print (show x)
  serializeInt32 x                             = print (show x)
  serializeInt64 x                             = print (show x)
  serializeUInt32 x                            = print (show x)
  serializeUInt64 x                            = print (show x)
  serializeSInt32 x                            = print (show x)
  serializeSInt64 x                            = print (show x)
  serializeFixed32 x                           = print (show x)
  serializeFixed64 x                           = print (show x)
  serializeSFixed32 x                          = print (show x)
  serializeSFixed64 x                          = print (show x)
  serializeBool x                              = print (showBool x)
  serializeString x                            = print (show x)
  serializeBytes x                             = print (show x)
  serializeEnumValue (MkEnumValueDescriptor name _) = print name
  startMessage                                 = do {
    (indent, buffer) <- get
    put (indent + 1, buffer ++ "{\n")
  }
  endMessage                                   = do {
    (indent, buffer) <- get
    put (minus indent 1, buffer ++ "}")
  }

export serializeToTextFormat : InterpMessage d -> String
serializeToTextFormat x = snd (execState
  (Protobuf.Serializer.serializeMessage {m=TextSerializer} x)
  (Z, ""))

export implementation Show (InterpMessage d) where
  show = serializeToTextFormat

--- Deserialization

--- Deserialization is implemented using the Lightyear monadic parser package.
--- TODO: commit to paths to give a better error stack.

TextDeserializer : Type -> Type
TextDeserializer = Parser

implementation Deserializer TextDeserializer where
  deserializeDouble = fail "Not Implemented"
  deserializeFloat = fail "Not Implemented"
  deserializeInt32 = assert_total $ do {
    chars <- many (satisfy isDigit)
    spaces
    case parseInteger (pack chars) of
      Nothing => fail "Could not parse Int32"
      Just x => return x
  }
  deserializeInt64 = fail "Not Implemented"
  deserializeUInt32 = fail "Not Implemented"
  deserializeUInt64 = fail "Not Implemented"
  deserializeSInt32 = fail "Not Implemented"
  deserializeSInt64 = fail "Not Implemented"
  deserializeFixed32 = fail "Not Implemented"
  deserializeFixed64 = fail "Not Implemented"
  deserializeSFixed32 = fail "Not Implemented"
  deserializeSFixed64 = fail "Not Implemented"
  deserializeBool = fail "Not Implemented"
  -- TODO: handle escape codes including \"
  deserializeString = assert_total $ do {
    char '"'
    chars <- many (satisfy (\c => c /= '"'))
    char '"'
    spaces
    return (pack chars)
  }
  deserializeBytes = deserializeString

  startMessage = assert_total $ token "{"

  maybeReadFieldNameOrNumber = assert_total $
    ((eof <|> token "}") *> return Nothing)
    <|> do {
      chars <- many (satisfy (\c => c /= ':' && not (isSpace c)))
      spaces
      token ":"
      return (Just (Left (pack chars)))
    }

  readEnumValueNameOrNumber = assert_total $ do {
    chars <- many (satisfy isAlpha)
    spaces
    return (Left (pack chars))
  }

  error e = fail (show e)

--TODO: strip initial whitespace
export deserializeFromTextFormat : String -> Either String (InterpMessage d)
deserializeFromTextFormat {d=d} str = parse (deserializeMessage {m=TextDeserializer} {d=d}) str
