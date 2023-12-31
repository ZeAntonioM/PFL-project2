module Header where

-- Instruction to be executed
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

-- Code to be executed by the machine
type Code = [Inst]

-- Possible values to be store inside of the stack and the state
data Value = IntValue Integer | TT | FF deriving (Eq, Show, Ord)

-- Stack of values representing the stack of the machine
type Stack  =  [Value]

-- State of the machine
type State =  [(String, Value)] 

-- Arithmetic expressions
data Aexp = Number Integer | Var String | AddE Aexp Aexp | SubE Aexp Aexp | MultE Aexp Aexp deriving Show

-- Boolean expressions
data Bexp = Boolean Bool | EqE Aexp Aexp | LeE Aexp Aexp | EqBexpE Bexp Bexp | NegE Bexp | AndE Bexp Bexp deriving Show

-- Statements to be interpreted by the compiler
data Stm = Aex Aexp | Bex Bexp | Assign String Aexp | Seq Stm Stm | If Stm [Stm] [Stm] | While Stm [Stm] deriving Show

-- Program to be interpreted by the compiler
type Program = [Stm]

-- Implementation of a binary tree
data Tree = Node String Tree Tree | Leaf  deriving Show