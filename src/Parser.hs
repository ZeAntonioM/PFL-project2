module Parser where
import Header
import Utils (inside, stringToNumber, isInteger, statement)


-- Function to extract the if condition from the if statement
ifCondition:: [String] -> [String]
ifCondition ("(":string) = inside string
ifCondition string = ifConditionAux string

-- Auxiliary function to extract the if condition from the if statement when the condition is not inside brackets
ifConditionAux:: [String] -> [String]
ifConditionAux ("then":string) = []
ifConditionAux (first:string) = first:ifConditionAux string

-- Function to extract the while condition from the while statement
whileCondition:: [String] -> [String]
whileCondition ("(":string) = inside string
whileCondition string = whileConditionAux string

-- Auxiliary function to extract the while condition from the while statement when the condition is not inside brackets
whileConditionAux:: [String] -> [String]
whileConditionAux ("do":string) = []
whileConditionAux (first:string) = first:whileConditionAux string

-- Function to calculate the level of precedence of a token
precedence:: String -> Integer
precedence [] = 0
precedence "not" = 2
precedence "*" = 3
precedence "+" = 4
precedence "-" = 4
precedence "<=" = 5
precedence "==" = 6
precedence "=" = 7
precedence "and" = 8
precedence _ = 1

--- Function to parse an arithmetic or boolean expression and transform it into a binary tree 
parseAex:: [String] -> Tree
parseAex string = parseAexAux string [] [] []

-- Auxiliary function to parse an arithmetic or boolean expression and transform it into a binary tree 
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

-- Function to parse an If statement and transform it into a binary tree 
parseIf:: [String ] -> (Tree, [String])
parseIf string= (Node "if" (parseAex cond ) right,rest)
    where cond = ifCondition string
          n = length cond + (if take 1 string == ["("] then 2 else 0)
          (right , rest) = parseIfElse (drop n string)

-- Auxiliary Function to parse the code to be executed in both the if and else condition and transform it into a binary tree
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
          n = length code +2

parseIfElse ("else":string) = (parseStatements code,drop n string)
        where code = statement string ++ [";"]
              n = length code 

-- Function to extract to parse and Assign statement and transform it into a binary tree
parseAssign:: String -> [String] -> (Tree, [String])
parseAssign var string = (Node ":=" (Node var Leaf Leaf) exp, rest) 
    where stm =  statement string
          exp = parseAex stm
          rest = drop (length stm +1) string

-- Function to extract to parse and While statement and transform it into a binary tree
parseWhile:: [String] -> (Tree, [String])
parseWhile ("do":"(":string) = (parseStatements code, drop n string)
    where code = inside string
          n = length code +2

parseWhile ("do":string) = (parseStatements code, drop n string)
    where code = statement string ++ [";"]
          n = length code

parseWhile string = (Node "while" exp right, rest)
    where cond = whileCondition string
          exp = parseAex cond
          n = length cond + (if take 1 string == ["("] then 2 else 0)
          code = drop n string
          (right, rest) = parseWhile code

-- Function to parse all the statements from a string and transform them into a binary tree
parseStatements:: [String] -> Tree
parseStatements [] = Leaf
parseStatements ("if":string) = Node "Seq" left (parseStatements rest) where (left, rest) = parseIf string
parseStatements ("while":string) =  Node "Seq" left (parseStatements rest) where (left, rest) = parseWhile string
parseStatements (var:":=":string) = Node "Seq" left (parseStatements rest) where (left, rest) = parseAssign var string
parseStatements string = Node "Seq" left (parseStatements rest) 
    where stm = statement string
          left = parseAex stm
          rest = drop (length stm +1) string


-- Function to transform a binary tree into a list of statements that can be interpret but the compiler
parseTree:: Tree -> Program
parseTree (Node "Seq" left Leaf) = parseTree left
parseTree (Node "Seq" left right) = parseTree left ++ parseTree right
parseTree (Node "if" left (Node "IfElse" c1 c2)) = [ If cond (parseTree c1) (parseTree c2)] where cond:rest = parseTree left  
parseTree (Node "while" left right) = [While cond (parseTree right)] where cond:rest = parseTree left 
parseTree (Node ":=" (Node var Leaf Leaf) right) = [Assign var exp] where Aex exp:rest = parseTree right 
parseTree (Node token left right) 
    | token =="+" =[Aex(AddE first second) ] 
    | token =="-" = [Aex(SubE first second) ] 
    | token == "*" = [Aex(MultE first second) ] 
    | token == "==" = [Bex(EqE first second) ] 
    | token == "<=" = [Bex(LeE first second) ] 
    where Aex first:rest = parseTree left
          Aex second:rest2 = parseTree right

parseTree (Node token left right) 
    | token =="=" =[Bex(EqBexpE first second) ] 
    | token =="not" =[Bex(NegE second) ] 
    | token =="and" =[Bex(AndE first second) ] 
    where Bex first:rest = parseTree left
          Bex second:rest2 = parseTree right

parseTree (Node token Leaf Leaf) 
    | isInteger token = [Aex(Number (stringToNumber token))]
    | token == "True" = [Bex (Boolean True)]
    | token == "False" = [Bex (Boolean False)]
    | otherwise = [Aex(Var token)]







