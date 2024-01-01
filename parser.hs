
data Tree = Node String Tree Tree | Leaf  deriving Show

inside:: [String] -> [String]
inside string = insideAux string 1

insideAux:: [String] -> Integer -> [String]
insideAux _ 0 = []
insideAux ("(":string) n = "(": insideAux string (n+1)
insideAux (")":string) 1 = []
insideAux (")":string) n = ")": insideAux string (n-1)
insideAux (x:string) n = x:insideAux string n

statement:: String -> String 
statement (';':string) = []
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






