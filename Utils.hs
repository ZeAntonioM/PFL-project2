module Utils where
import Header
import Data.Char (isDigit)

-- Function to check if a value is an integer
isInt :: Value -> Bool
isInt (IntValue _) = True
isInt _ = False

-- Function to check if a value is a boolean
isBool :: Value -> Bool
isBool TT = True
isBool FF = True
isBool _ = False

-- Function to find the first two integers in a stack
findFirst2Int :: Stack -> (Value, Value)
findFirst2Int stack = (first, second)
  where 
    aux = [y | y<- stack ,y /= TT && y /=FF ] 
    [first,second] =take 2 aux 

-- Function to remove the first occurrence of a pair with a key from a list 
removeKey::Eq a => String -> [(String, a)] -> [(String, a)] 
removeKey _ [] = []  -- Base case: empty list, nothing to remove
removeKey key ((k,value):ys)
  | key == k    = ys    -- Found the element, skip it
  | otherwise = (k,value) : removeKey key ys  -- Keep the current element and continue with the rest of the list

-- Function to remove the first occurrence of an element from a list
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []  -- Base case: empty list, nothing to remove
removeFirst x (y:ys)
  | x == y    = ys    -- Found the element, skip it
  | otherwise = y : removeFirst x ys  -- Keep the current element and continue with the rest of the list

-- Function to find the value that corresponds to a key in state
findValueFromKey:: String -> State -> Value
findValueFromKey _ [] = error "Run-time error"
findValueFromKey key ((k,value):rest)
  | key == k =value
  | otherwise = findValueFromKey key rest
  
-- Function to transform a value into a string
value2Str :: Value -> String
value2Str value = case value of
  IntValue n -> show n
  TT -> "True"
  FF -> "False"

-- Function to transform a pair of a variable and a value into a string
pair2Str :: ([Char], Value) -> [Char]
pair2Str (var, value) = var ++ "=" ++ value2Str value

-- Function to sort the state using quicksort
stateSort :: State -> State
stateSort [] = []
stateSort ((key,value):xs) = stateSort lower ++ [(key,value)] ++ stateSort higher
  where 
    lower = [(x,y) | (x,y)<-xs, x<=key] 
    higher = [(x,y) | (x,y)<-xs, x>key]

-- Function to converts String to Number
stringToNumber :: String -> Integer
stringToNumber str = case reads str of
  [(num, "")] -> num
  _           -> error "Run time error"

-- Function to verify if a string represent a number
isInteger :: String -> Bool
isInteger = all isDigit

-- Function to extract the contents inside of brackets
inside:: [String] -> [String]
inside string = insideAux string 1

-- Auxiliar function to extract the contents inside of brackets
insideAux:: [String] -> Integer -> [String]
insideAux _ 0 = []
insideAux ("(":string) n = "(": insideAux string (n+1)
insideAux (")":string) 1 = []
insideAux (")":string) n = ")": insideAux string (n-1)
insideAux (x:string) n = x:insideAux string n

-- Function to extract the contents of a statement
statement:: [String] -> [String] 
statement (";":string) = []
statement (x:string) = x:statement string