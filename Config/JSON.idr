-- ---------------------------------------------------------------- [ JSON.idr ]
-- Description : Parse JSON files.
--               This code was borrowed and improved from lightyear examples.
--
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Config.JSON

import public Data.AVL.Dict

import Effects
import Effect.File

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import public Config.Error

import Config.Parse.Utils
import Config.Parse.Common

%access private

-- ------------------------------------------------------------------- [ Model ]

public export
data JsonValue = JsonString String
               | JsonNumber Double
               | JsonBool Bool
               | JsonNull
               | JsonArray (List JsonValue)
               | JsonObject (Dict String JsonValue)

public export
Show JsonValue where
  show (JsonString s)   = show s
  show (JsonNumber x)   = show x
  show (JsonBool True ) = "true"
  show (JsonBool False) = "false"
  show  JsonNull        = "null"
  show (JsonArray  xs)  = show xs
  show (JsonObject xs)  =
      "{" ++ unwords (intersperse "," (map fmtItem $ Dict.toList xs)) ++ "}"
    where
      fmtItem (k, v) = show k ++ " : " ++ show v

-- ------------------------------------------------------------------ [ Parser ]

-- QUESTION/DISCUSSION: xxquoted and xxquoted' are brought in from
-- lightyear and fiddled with to use xxstringchar.
-- It is a bit ugly that xxquoted' uses xxstringchar to parse tokens
-- but doesn't use that same parser to recognise the end
-- character.

hexToInt : Char -> Int
hexToInt '0' = 0
hexToInt '1' = 1
hexToInt '2' = 2
hexToInt '3' = 3
hexToInt '4' = 4
hexToInt '5' = 5
hexToInt '6' = 6
hexToInt '7' = 7
hexToInt '8' = 8
hexToInt '9' = 9
hexToInt 'A' = 10
hexToInt 'B' = 11
hexToInt 'C' = 12
hexToInt 'D' = 13
hexToInt 'E' = 14
hexToInt 'F' = 15

hexInt : Monad m => ParserT String m Int
hexInt = (hexToInt . toUpper) <$> hexDigit

unicodeHexchar : Monad m => ParserT String m Char
unicodeHexchar = do
  d1 <- hexInt
  d2 <- hexInt
  d3 <- hexInt
  d4 <- hexInt
  pure $ cast $ d1 * 16 * 16 * 16 + d2 * 16 * 16 + d3 * 16 + d4

-- this list is non-exhaustive: it is the escapes that I've come
-- across in data I've tried to parse.
xxescapedchars : Monad m => ParserT String m Char
xxescapedchars = (char '"' *> pure '"')
             <|> (char 'n' *> pure '\n')
             <|> (char 'u' *> unicodeHexchar)
             <|> do { c <- anyChar ; fail $ "Unexpected string escape character: " ++ show c }


-- QUESTION/DISCUSSION: why is this type not Parser Char?
-- what does Parser Char expand to (if anything?)
-- I think Parser forces the m to be identity but 'some'
-- wants it to be polymorphic in m?
xxstringchar : Monad m => ParserT String m Char
xxstringchar = ((satisfy $ \c => (c /= '"') && ( c /= '\\')) <?> "unescaped string character")
           <|> (char '\\' >! (xxescapedchars <?> "string escape selector(?phrasing)"))
           <?> "valid string character"

-- QUESTION/DISCUSSION does that >! after the \\ cause
-- better error messages by stopping backtracking once
-- we are deep in a string? (so that we report a broken
-- string rather than backtracking and finding nothing?)
-- It does!
-- actually making that pruning happen elsewhere might
-- make debugging/error messages easier to understand,
-- rather than so much backtracking happening?


||| Collect the literal string contained between two characters
xxquoted' : Monad m => Char -> Char -> ParserT String m String
xxquoted' l r = map pack $ between (char l) (lexeme $ char r) (many xxstringchar)
-- this 'many' is changed from 'some' in the library version of `quoted'`
-- so as to accept null strings
-- It would be good if we were strict on the left hand char, because
-- JSON doesn't need backtracking here and we could focus errors
-- a bit more?

||| Literal string between two identical characters
xxquoted : Monad m => Char -> ParserT String m String
xxquoted c = xxquoted' c c

jsonString : Parser String
jsonString = xxquoted '"' <?> "JSON String XXXquoted"

jsonNumber : Parser Double
jsonNumber = map scientificToFloat parseScientific <?> "JSON Number"

jsonBool : Parser Bool
jsonBool  =  (char 't' >! string "rue"  *> pure True)
         <|> (char 'f' >! string "alse" *> pure False)
         <?> "JSON Bool"

jsonNull : Parser ()
jsonNull = (char 'n' >! string "ull" >! pure ()) <?> "JSON Null"

mutual
  jsonArray : Parser (List JsonValue)
  jsonArray = brackets (commaSep jsonValue) <?> "JSON Array"

  keyValuePair : Parser (String, JsonValue)
  keyValuePair = do
      key <- spaces *> jsonString <* spaces
      commitTo $ do
        colon
        value <- jsonValue
        pure (key, value)
    <?> "JSON KV Pair"

-- this commitTo changes from this:
-- (in the case that there's an empty string, 1:12331 is "key":"")
-- at a time when this parser can't accept empty strings
{-
at 1:83 expected:
  JSON Object
at 1:12331 expected:
  token "}"
at 1:12331 expected:
  string "}"
at 1:12331 expected:
  character '}'
at 1:12331 expected:
  a different token
-}
-- to
{-
at 1:83 expected:
  JSON Object
at 1:12333 expected:
  JSON KV Pair
at 1:12346 expected:
  JSON Value
... followed by a bazillion lines of
    trying to parse a valid JSON value
-}
-- which is arguably better because it makes
-- further progress and gives the error closer
-- to where the error happened?


  jsonObject : Parser (Dict String JsonValue)
  jsonObject = map fromList $ braces (commaSep (keyValuePair)) <?> "JSON Object"

  jsonValue' : Parser JsonValue
  jsonValue' =  (map JsonString jsonString)
            <|> (map JsonNumber jsonNumber)
            <|> (map JsonBool   jsonBool)
            <|> (pure JsonNull <* jsonNull)
            <|>| map JsonArray  jsonArray
            <|>| map JsonObject jsonObject

  jsonValue : Parser JsonValue
  jsonValue = spaces *> jsonValue' <* spaces <?> "JSON Value"

export
parseJSONFile : Parser JsonValue
parseJSONFile = (map JsonArray jsonArray)
            <|> (map JsonObject jsonObject)
            <?> "JSON Files"



export
toString : JsonValue -> String
toString doc = show doc

export
fromString : String -> Either ConfigError JsonValue
fromString str =
    case parse parseJSONFile str of
      Left err  => Left (PureParseErr err)
      Right doc => Right doc

-- -------------------------------------------------------------------- [ Read ]
export
readJSONConfig : String -> Eff (Either ConfigError JsonValue) [FILE ()]
readJSONConfig = readConfigFile parseJSONFile

-- --------------------------------------------------------------------- [ EOF ]
