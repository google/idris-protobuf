module Protobuf.TextFormat

import Control.Monad.State
import Data.String

import Protobuf
import Protobuf.Deserializer
import Protobuf.Serializer

%default total

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

--- We construct an error type

-- A text deserializer uses the Provider monad to handle errors, and the state
-- is the fixed string being parsed, along with a cursor into the string.
TextDeserializer : Type -> Type
TextDeserializer = StateT (Int, String) Provider

returnError : String -> TextDeserializer a
returnError s = ST (\x => Error s)

partial seek : (pos : Int) -> (cond : Char -> Bool) -> String -> Int
seek pos cond s =
  if pos < prim_lenString s then
    if cond (prim__strIndex s pos) then
      pos
    else
      seek (pos + 1) cond s
  else
    pos

skipWhitespace : TextDeserializer ()
skipWhitespace = assert_total (do {
  (i, s) <- get
  put (seek i (\x => not (isSpace x)) s, s)
})

peek : TextDeserializer (Maybe Char)
peek = assert_total (do {
  (i, s) <- get
  if i >= prim_lenString s then
    return Nothing
  else
    return (Just (prim__strIndex s i))
})

requireChar : Char -> TextDeserializer ()
requireChar = assert_total (\c => do {
  (i, s) <- get
  if i >= prim_lenString s then
    returnError ("Reached end of string while searching for " ++ (show c))
  else
    let c' = prim__strIndex s i in
      if c' == c then
        put (i + 1, s)
      else
        returnError ("Expected " ++ (show c) ++ ", got " ++ (show c'))
})

consumeUpToWhitespaceAndParse : (String -> Maybe a) -> TextDeserializer a
consumeUpToWhitespaceAndParse  = assert_total (\f => do {
  (i, s) <- get
  i' <- return (seek i (\x => isSpace x || x == '}') s)
  put (i', s)
  consumed <- return (prim__strSubstr i (i' - i) s)
  maybe (returnError "Failed to parse double") (\x => return x) (f consumed)
})

consumeFieldName : TextDeserializer String
consumeFieldName  = assert_total (do {
  (i, s) <- get
  i' <- return (seek i (\x => x == ':') s)
  put (i', s)
  return (prim__strSubstr i (i' - i) s)
})

parseBool : String -> Maybe Bool
parseBool s =
  if s == "true" then
    Just True
  else
    if s == "false" then
      Just False
    else
      Nothing

implementation Deserializer TextDeserializer where
  deserializeDouble = consumeUpToWhitespaceAndParse parseDouble
  deserializeFloat = consumeUpToWhitespaceAndParse parseDouble
  deserializeInt32 = consumeUpToWhitespaceAndParse parseInteger
  deserializeInt64 = consumeUpToWhitespaceAndParse parseInteger
  deserializeUInt32 = consumeUpToWhitespaceAndParse parsePositive
  deserializeUInt64 = consumeUpToWhitespaceAndParse parsePositive
  deserializeSInt32 = consumeUpToWhitespaceAndParse parseInteger
  deserializeSInt64 = consumeUpToWhitespaceAndParse parseInteger
  deserializeFixed32 = consumeUpToWhitespaceAndParse parsePositive
  deserializeFixed64 = consumeUpToWhitespaceAndParse parsePositive
  deserializeSFixed32 = consumeUpToWhitespaceAndParse parseInteger
  deserializeSFixed64 = consumeUpToWhitespaceAndParse parseInteger
  deserializeBool = consumeUpToWhitespaceAndParse parseBool
  deserializeString = assert_total (do {
    requireChar '"'
    (i, s) <- get
    i' <- return (seek i (\x => x == '"') s)
    put (i', s)
    requireChar '"'
    return (prim__strSubstr i (i' - i) s)
  })
  deserializeBytes = deserializeString

  startMessage = do {
    skipWhitespace
    requireChar '{'
  }

  maybeReadFieldNameOrNumber = do {
    skipWhitespace
    c <- peek
    if c == Nothing then do {
      return Nothing
    } else if c == Just '}' then do {
      requireChar '}'
      return Nothing
    } else do {
      name <- consumeFieldName
      skipWhitespace
      requireChar ':'
      skipWhitespace
      return (Just (Left name))
    }
  }

  readEnumValueNameOrNumber = do {
    skipWhitespace
    name <- consumeUpToWhitespaceAndParse Just
    return (Left name)
  }

  error e = returnError (show e)


export deserializeFromTextFormat : String -> Provider (InterpMessage d)
deserializeFromTextFormat {d=d} str = do {
  (x, state) <- runStateT {s=(Int, String)} (deserializeMessage {m=TextDeserializer} {d=d}) (0, str)
  return x
}
