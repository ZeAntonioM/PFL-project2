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


-- Part 2
 
-- Function to transform an arithmetic expression into code
compA :: Aexp -> Code
compA (Number n) = [Push n] -- If the expression is a number, returns the instruction Push n
compA (Var x) = [Fetch x] -- If the expression is a variable, returns the instruction Fetch x
compA (AddE a1 a2) = compA a2 ++ compA a1 ++ [Add] -- If the expression is an addition, returns the compiled code of the second expression, then the first expression, then the instruction Add
compA (SubE a1 a2) = compA a2 ++ compA a1 ++ [Sub] -- If the expression is a subtraction, returns the compiled code of the second expression, then the first expression, then the instruction Sub
compA (MultE a1 a2) = compA a2 ++ compA a1 ++ [Mult] -- If the expression is a multiplication, returns the compiled code of the second expression, then the first expression, then the instruction Mult

-- Function to transform a boolean expression into code
compB :: Bexp -> Code
compB (Boolean b) = if b then [Tru] else [Fals] -- If the expression is a boolean, returns the instruction Tru if it is True, Fals otherwise
compB (EqE a1 a2) = compA a2 ++ compA a1 ++ [Equ] -- If the expression is an arithmetic equality, returns the compiled code of the second expression, then the first expression, then the instruction Equ
compB (EqBexpE a1 a2) = compB a2 ++ compB a1 ++ [Equ] -- If the expression is an boolean equality, returns the compiled code of the second expression, then the first expression, then the instruction Equ
compB (LeE a1 a2) = compA a2 ++ compA a1 ++ [Le] -- If the expression is an arithmetic less than or equal, returns the compiled code of the second expression, then the first expression, then the instruction Le
compB (NegE b) = compB b ++ [Neg] -- If the expression is a negation, returns the compiled code of the expression, then the instruction Neg
compB (AndE b1 b2) = compB b2 ++ compB b1 ++ [And] -- If the expression is a conjunction, returns the compiled code of the second expression, then the first expression, then the instruction And

-- Function to transform a statement into code
compile :: Program -> Code
compile [] = [] -- If the program is empty, returns an empty list
compile ((Aex a):rest) = compA a ++ compile rest -- If the first element is an arithmetic expression, returns the compiled code of the expression and the compiled code of the rest of the program
compile ((Bex b):rest) = compB b ++ compile rest -- If the first element is a boolean expression, returns the compiled code of the expression and the compiled code of the rest of the program
compile ((Assign x a):rest) = compA a ++ [Store x] ++ compile rest -- If the first element is an assignment, returns the compiled code of the arithmetic expression, the instruction Store x and the compiled code of the rest of the program
compile ((If (Bex b) s1 s2):rest) = compB b ++ [Branch (compile s1) (compile s2)] ++ compile rest -- If the first element is an if statement, returns the compiled code of the boolean expression, the instruction Branch, that receives the compiled code of the first statement and the compiled code of the second statement, and the compiled code of the rest of the program 
compile ((While (Bex b) s):rest) = Loop (compB b) (compile s):compile rest -- If the first element is a while statement, returns the instruction Loop, that receives the compiled code of the boolean expression and the compiled code of the statement, and the compiled code of the rest of the program

-- Function to transform a string into a Program
parse :: String -> Program
parse s = parseTree (parseStatements (lexer s) )

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

main :: IO ()
main = undefined


          


