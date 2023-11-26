module Main where

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Integer
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq, Ord)

main :: IO ()
main = putStrLn "Hello, Haskell!"
