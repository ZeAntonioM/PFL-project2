module Lexer where
import Data.Char

-- Function to transform a string in a list of tokens
lexer :: String -> [String]
lexer [] = []
lexer (x:xs)
  | x == ';' = ";" : lexer xs 
  | x == '(' = "(" : lexer xs
  | x == ')' = ")" : lexer xs
  | x == '+' = "+" : lexer xs
  | x == '-' = "-" : lexer xs
  | x == '*' = "*" : lexer xs
  | x == '=' = if lexer (take 1 xs)  == ["="] then "==":lexer (drop 1 xs) else "=" : lexer xs
  | x:take 1 xs == ":=" = ":=" : lexer (drop 1 xs)
  | x:take 1 xs == "<=" = "<=" : lexer (drop 1 xs)
  | x == '¬' = "not" : lexer xs
  | isNumber x = number : lexer (drop (length number - 1) xs) 
  | isLetter x = word : lexer (drop (length word - 1) xs)
  | isSpace x = lexer xs
  | otherwise = error "Run-time error"
    where
      number = getInt (x:xs) 
      word = getWord (x:xs)

-- Function to extract a Integer from a string
getInt :: String -> String
getInt [] = []
getInt (x:xs)
  | x == '0' = x : getInt xs
  | x == '1' = x : getInt xs
  | x == '2' = x : getInt xs
  | x == '3' = x : getInt xs
  | x == '4' = x : getInt xs
  | x == '5' = x : getInt xs
  | x == '6' = x : getInt xs
  | x == '7' = x : getInt xs
  | x == '8' = x : getInt xs
  | x == '9' = x : getInt xs
  | otherwise = []

-- Function to extract a word from a string
getWord :: String -> String
getWord [] = []
getWord (x:xs)
  | isLetter x = x : getWord xs
  | otherwise = []