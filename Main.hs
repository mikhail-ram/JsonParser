module Main where

import Data.Char
import System.Environment
import Control.Applicative

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Integer -- TODO: Add support for floats
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

-- TODO: Add proper error reporting
newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
}

instance Functor Parser where
  fmap f (Parser p) = Parser g
    where
      g input = do
        (input', x) <- p input
        Just (input', f x)

instance Applicative Parser where
  pure x = Parser g
    where g input = Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser g
    where
      g input = do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser g
    where g input = p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x    = Just (ys, x)
      | otherwise = Nothing
    f []          = Nothing

stringP :: String -> Parser String
stringP = traverse charP

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true"  = JsonBool True
    f "false" = JsonBool False
    f _       = undefined -- This should never happen

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser g
  where
    g input = Just (rest, token)
      where (token, rest) = span f input

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser g
  where
  g input = do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where f ds = JsonNumber $ read ds

-- TODO: add support for string escaping
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> array
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue
    array = charP '[' *> ws *> elements <* ws <* charP ']'

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject <$> object
  where
    pair = ignoreSecond <$> stringLiteral <*> (ws *> charP ':' <* ws) <*> jsonValue
      where ignoreSecond key _ value = (key, value)
    object = charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}'

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

main :: IO ()
main = do
  (filePath:_) <- getArgs
  result <- parseFile filePath jsonValue
  print result
