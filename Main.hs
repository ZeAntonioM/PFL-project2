module Main where
import Parser ( parseTree, parseStatements)
import Lexer (lexer)
import Header
import Utils
import Data.Char
-- Part 1


-- Function to create an empty stack
createEmptyStack :: Stack
createEmptyStack =  [] 

-- Function to transform a stack into a string
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [value] = value2Str value
stack2Str (value:rest) = value2Str value ++ "," ++ stack2Str rest

-- Function to create an empty state
createEmptyState :: State
createEmptyState = []

-- Function to transform a state into a string
state2Str :: State -> String
state2Str state = state2StrAux (stateSort state)

-- Auxiliary function to transform a sorted state into a string
state2StrAux :: [([Char], Value)] -> [Char]
state2StrAux [] = ""
state2StrAux [curr] = pair2Str curr
state2StrAux  (curr:rest) = pair2Str curr ++ "," ++ state2Str rest



-- Function to execute an instruction
execute :: Inst -> Stack -> State -> (Stack, State)

-- Instruction Push n: push the integer n on the stack
execute (Push n) stack state = ( IntValue n:stack, state)

-- Instructions Tru and Fals: push the boolean value on the stack
execute Tru stack state = ( TT:stack, state)
execute Fals stack state = ( FF:stack, state)

-- Instruction And: pop two boolean values from the stack, push their conjunction
execute And stack state
  | length stack < 2 = error "Run-time error"
  | not(isBool first) || not (isBool second) = error "Run-time error"
  | otherwise =
    ((if first == TT && second == TT then TT else FF):rest, state)
  where
    first:second:rest = stack

-- Instruction Neg: pop a boolean value from the stack, push its negation
execute Neg stack state
  | length stack < 1 = error "Run-time error"
  | not(isBool first) = error "Run-time error"
  | otherwise =
    ((if first == TT then FF else TT):rest, state)
  where
    first:rest = stack

-- Instructions Add, Mult: pop two integer values from the stack, push their sum/product
execute Add stack state = ( IntValue( x + y):stack'', state) 
  where
    (IntValue x,IntValue y) = findFirst2Int stack
    stack' = removeFirst (IntValue x) stack
    stack'' = removeFirst (IntValue y) stack'

execute Mult stack state = ( IntValue( x * y):stack'', state) 
  where
    (IntValue x,IntValue y) = findFirst2Int stack
    stack' = removeFirst (IntValue x) stack
    stack'' = removeFirst (IntValue y) stack'

-- Instruction Sub: pop the first two values from the stack, push their difference if they are integers
execute Sub stack state
  | length stack < 2 = error "Run-time error"
  | not(isInt first) || not (isInt second) = error "Run-time error"
  | otherwise =
    (IntValue (x - y) : rest, state)
  where
    first:second:rest = stack
    IntValue x = first
    IntValue y = second

-- Instruction Equ: pop the first two values from the stack, push their equality if they are both integers or booleans
execute Equ stack state
  | length stack < 2 = error "Run-time error"
  | ( isInt first && isBool second ) || ( isBool first && isInt second )  = error "Run-time error"
  | otherwise =
    ((if first == second then TT else FF):rest, state)
  where
    first:second:rest = stack

-- Instruction Le: pop the first two values from the stack, push their less than or equal if they are both integers
execute Le stack state 
  | length stack < 2 = error "Run-time error"
  | not(isInt first) || not (isInt second) = error "Run-time error"
  | otherwise =
    ((if first <= second then TT else FF):rest, state)
  where
    first:second:rest = stack
  
-- Instruction Fetch x: Finds the value of x in state and pushes it on the stack
execute (Fetch key)  stack state = (findValueFromKey key state: stack, state)

-- Instruction Store x: Pops a value from the stack and stores it in x in state
execute (Store key)  (first:rest) state = (rest, (key,first):state') where state' = removeKey key state

-- Instruction Branch code1 code2: Pops a boolean value from the stack, executes code1 if it is True, code2 otherwise
execute (Branch code1 code2) (first:rest) state
  | first == TT = ( stack', state')
  | first == FF = ( stack'', state'')
  | otherwise = error "Run-time error"
  where
    (_, stack', state') = run (code1 ,rest, state) 
    (_, stack'', state'') = run (code2 ,rest, state) 

-- Instruction Noop: dummy instruction, just return the stack and state
execute Noop stack state = (stack, state)

-- Instruction Loop code1 code2: Runs code1, then code2, then code1, then code2, etc. until the top of the stack is False
execute (Loop code1 code2) stack state = (newstack, newstate)
  where
    (_, newstack, newstate) = run(code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]], stack, state) 
  
-- Function that executes instructions in the low-level machine
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([] , stack, state )= ([], stack, state)
run (instruction: rest, stack, state) = run ( rest, stack', state')
  where (stack', state') = execute instruction stack state


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

{--
Examples:
testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
testAssembler [Push (-20),Push (-21), Le] == ("True","")
testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

If you test:
testAssembler [Push 1,Push 2,And]
You should get an exception with the string: "Run-time error"

If you test:
testAssembler [Tru,Tru,Store "y", Fetch "x",Tru] 
You should get an exception with the string: "Run-time error"
--}


-- Part 2
 
-- TODO: Define the types Aexp, Bexp, Stm and Program


compA :: Aexp -> Code
compA (Number n) = [Push n]
compA (Var x) = [Fetch x]
compA (AddE a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (SubE a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (MultE a1 a2) = compA a2 ++ compA a1 ++ [Mult]

compB :: Bexp -> Code
compB (Boolean b) = if b then [Tru] else [Fals]
compB (EqE a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (EqBexpE a1 a2) = compB a2 ++ compB a1 ++ [Equ]
compB (LeE a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (NegE b) = compB b ++ [Neg]
compB (AndE b1 b2) = compB b2 ++ compB b1 ++ [And]

compile :: Program -> Code
compile [] = []
compile ((Aex a):rest) = compA a ++ compile rest
compile ((Bex b):rest) = compB b ++ compile rest
compile ((Assign x a):rest) = compA a ++ [Store x] ++ compile rest
compile ((If (Bex b) s1 s2):rest) = compB b ++ [Branch (compile s1) (compile s2)] ++ compile rest 
compile ((While (Bex b) s):rest) = Loop (compB b) (compile s):compile rest

parse :: String -> Program
parse s = parseTree (parseStatements (lexer s) )

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")


main :: IO ()
main = undefined


          


