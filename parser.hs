data Tree = Node String Tree Tree | Leaf  deriving Show


inside:: [String] -> [String]
inside string = insideAux string 1

insideAux:: [String] -> Integer -> [String]
insideAux _ 0 = []
insideAux ("(":string) n = "(": insideAux string (n+1)
insideAux (")":string) 1 = []
insideAux (")":string) n = ")": insideAux string (n-1)
insideAux (x:string) n = x:insideAux string n

statement:: [String] -> [String] 
statement (";":string) = []
statement (x:string) = x:statement string

precedence:: String -> Integer
precedence [] = 0
precedence "not" = 2
precedence "*" = 3
precedence "+" = 4
precedence "<=" = 5
precedence "==" = 6
precedence "=" = 7
precedence _ = 1

parseAex:: [String] -> Tree
parseAex string = parseAexAux string [] [] []

parseAexAux:: [String] -> String ->[String] ->[String] -> Tree
parseAexAux ("(":string) token left right 
    | (length string == (n+1)) && (token =="") =parseAexAux (take n string)  "" left right 
    | token=="" = parseAexAux (drop (n+1) string) "" (["("]++inside_++[")"]) right 
    | otherwise = parseAexAux (drop (n+1) string) token left (right ++ (["("]++inside_++[")"])) 
    where  inside_ = inside string
           n = length inside_

parseAexAux [] x [] [] = Node x Leaf Leaf
parseAexAux [] x left right = Node x (parseAexAux left [] [] []) (parseAexAux right [] [] [])
parseAexAux (token:string) "" left right = parseAexAux string token left right
parseAexAux (token:string) x left right | precedence token > precedence x = parseAexAux string token (left ++[x]++ right) []
                                        | otherwise = parseAexAux string x left (right++[token])


parseIf:: [String ] -> (Tree, [String])
parseIf ("(":string )= (Node "if" (parseAex cond ) right,rest)
    where cond = inside string
          n = length cond + 1
          (right , rest) = parseIfElse (drop n string)

parseIfElse:: [String] -> (Tree,[String])
parseIfElse ("then":"(":string) =(Node "IfElse" (parseStatements code) right, rest)
    where code = inside string
          n = length code +1
          (right , rest) = parseIfElse (drop n string)
parseIfElse ("then":string) = (Node "IfElse" (parseStatements code) right, rest)
        where code = statement string ++ [";"]
              n = length code
              (right , rest) = parseIfElse (drop n string)
parseIfElse ("else":"(":string) = (parseStatements code, drop n string) 
    where code = inside string
          n = length code +1

parseIfElse ("else":string) = (parseStatements code,drop n string)
        where code = statement string ++ [";"]
              n = length code 

parseAssign:: String -> [String] -> (Tree, [String])
parseAssign var string = (Node ":=" (Node var Leaf Leaf) exp, rest) 
    where stm =  statement string
          exp = parseAex stm
          rest = drop (length stm +1) string

parseWhile:: [String] -> (Tree, [String])
parseWhile ("(":string) = (Node "while" exp right, rest)
    where cond = inside string
          exp = parseAex cond
          n = length cond +1
          code = drop n string
          (right, rest) = parseWhile code


parseWhile ("do":"(":string) = (parseStatements code, drop n string)
    where code = inside string
          n = length code +1

parseWhile ("do":string) = (parseStatements code, drop n string)
    where code = statement string ++ [";"]
          n = length code
          



parseStatements:: [String] -> Tree
parseStatements [] = Leaf
parseStatements ("if":string) = Node "Seq" left (parseStatements rest) where (left, rest) = parseIf string
parseStatements ("while":string) =  Node "Seq" left (parseStatements rest) where (left, rest) = parseWhile string
parseStatements (var:":=":string) = Node "Seq" left (parseStatements rest) where (left, rest) = parseAssign var string
parseStatements string = Node "Seq" left (parseStatements rest) 
    where stm = statement string
          left = parseAex stm
          rest = drop (length stm +1) string









