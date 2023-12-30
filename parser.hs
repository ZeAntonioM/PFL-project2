--parse :: String -> Program
parse = undefined -- TODO

lexer :: String -> [String]
lexer (x:xs)
  | x == ' ' = lexer xs
  | x == ';' = ";" : lexer xs
  | x == '(' = "(" : lexer xs
  | x == ')' = ")" : lexer xs
  | x == '+' = "+" : lexer xs
  | x == '-' = "-" : lexer xs
  | x == '*' = "*" : lexer xs
  | x == '=' = if lexer (take 1 xs)  == ["="] then "==":lexer xs else "=" : lexer xs
  | x:take 1 xs == "<=" = "<=" : lexer (drop 1 xs)
  | x:take 1 xs == "if" = "if" : lexer (drop 1 xs)
  | x:take 3 xs == "then" = "then" : lexer (drop 3 xs)
  | x:take 4 xs == "else" = "else" : lexer (drop 4 xs)
  | x:take 5 xs == "while" = "while" : lexer (drop 5 xs)
  | x:take 1 xs == "do" = "do" : lexer (drop 1 xs)
  | x == '¬' = "¬" : lexer xs
  | x:take 1 xs == ":=" = ":=" : lexer (drop 1 xs)
