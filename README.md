# PFL-TP2

## Group T04_G14

- Francisco Miguel Correia Mariano Pinheiro Cardoso - up202108794 - 50%
- José António Pereira Martins - up202108794 - 50%

## Implementation strategy

To implement the whole work, we used the template file that was provided in moodle, and from there, we implemented what was necessary in each of the parts:

### Part 1 - Implementation

The goal of this part was to implement a way to get a low-level machine to run given a specific configuration (c, e, s), c being the configuration, e being the evaluation stack and s being the storage. 
In the template file, we were given the possible instructions that we could use to implement the machine, and 4 functions that we should implement: `createEmptyStack`, `stack2String`, `createEmptyState`, `state2String` and `run`.

First of all, we had to ensure the possible types for the machine to read, which are the Int and the Bool. For this, we created a data type called `Value`, which can be either an `IntValue`, or the Boolean Values `TT` or `FF`. 
Then, understanding what a Stack is, we created a type called `Stack`, which is a list of `Values`.
Finally, we created a type called `State`, which is a list of tuples, where the first element is a `variable`(String) and the second element is a `Value`. 

Defined all the types and the Values, we started to implement the functions.

- `createEmptyStack`: This function creates an empty stack, which is used to initialize the evaluation stack. To do this, we just return an empty list.

- `createEmptyState`: This function creates an empty state, which is used to initialize the machine state. To do this, we just return an empty list.

- `stack2Str`: This function converts a stack to a string, which is used to transform the stack into a string. For this, we recursively go through the stack, and for each element, we transform the Value into a string with an auxiliary function called `value2Str`, add a comma, and concatenate it with the rest of the stack. There are two base cases, one for the empty stack, and another for the last element of the stack, where we don't want to add a comma.

- `value2Str`: This function converts a Value into a string. It's a simples case of pattern matching: 
  - If the `Value` is an `IntValue`, we just use the `show` function to convert it into a string.
  - If the `Value` is `TT`, we return the string "true".
  - If the `Value` is `FF`, we return the string "false".

- `state2Str`: This function converts a state to a string. All the pairs of the state must be ordered by the variable name in alphabetical order. To do so, we simply call an auxiliary function called `state2StrAux` that receives  state ordered by other auxiliary function called `stateSort`.

- `stateSort`: This function sorts the state by the variable name in alphabetical order. To do this, we used a quicksort method, where it returns a list of tuples in the following order: lower ++ [first] ++ higher. 
    - The `lower` list is the tuples that whose variable name is lower than the first element of the list.
    - The `higher` list is the tuples that whose variable name is higher than the first element of the list.
    - The `first` element is the first element of the list.

- `state2StrAux`: This function converts a state to a string. It is said that each variable-value pair is represented without spaces and using an "=". To do this, we recursively go through the state, and for each element, we call an auxiliary function called `pair2Str`, add a comma, and concatenate it with the rest of the state. There are two base cases, one for the empty state, and another for the last element of the state, where we don't want to add a comma.

- `pair2Str`: This function converts a pair of a state to a string. To do this, we just concatenate the variable name with the value, and add a "=" between them.

- `run`: This function executes instructions in the low-level machine. To do this, we recursively go through the instructions, and for each instruction, we update the stack and the state using the auxiliary function `execute`. At the end, we return the final (c, e, s).

- `execute`: This function executes an instruction in the low-level machine. it receives the Instruction, the stack and the state, and must return a tuple with the updated (stack, state). There were a lot of instructions defined, and, in order to not get the code too confusing, we didn't use a case of pattern matching, but instead we implemented a different `execute` function for each instruction. The instructions implemented were the following:
  - `Push`: For this instruction, we just add the IntValue to the top of the stack.
  - `Tru`: For this instruction, we just add the `TT` to the top of the stack.
  - `Fals`: For this instruction, we just add the `FF` to the top of the stack.
  - `And`: For this instruction, we first check if the stack has at least 2 elements and if the top 2 elements are both Boolean Values. To do so, we use the auxiliary function `isBool`. Then, we check if both the top 2 elements are `TT`. If so, we add `TT` to the rest of the stack. If not, we add `FF` to the rest of the stack.
  - `Neg`: For this instruction, we first check if the stack has at least 1 element and if the top element is a Boolean Value. To do so, we use the auxiliary function `isBool`. Then, we check if the top element is `TT`. If so, we add `FF` to the rest of the stack. If not, we add `TT` to the rest of the stack.
  - `Add`: For this instruction it was said that we need to get the top two integer values from the stack. In order to do so, we use the auxiliary function `findFirst2Int`. Then, we remove the two top integer values from the stack using a function called `removeFirst`, and add the sum of those two values to the rest of the stack.
  - `Mult`: Just like the `Add` instruction, we need to get the top two integer values from the stack. In order to do so, we use the auxiliary function `findFirst2Int`. Then, we remove the two top integer values from the stack using a function called `removeFirst`, and add the multiplication of those two values to the rest of the stack.
  - `Sub`: For this instruction, we first check if the stack has at least 2 elements and if the top 2 elements are both IntValues. To do so, we use the auxiliary function `isInt`. If so, we get the top two values from the stack and add the subtraction of those two values to the rest of the stack. 
  - `Equ`: For this instruction, we first check if the stack has at least 2 elements and if the top 2 elements have the same type using the `isInt` and `isBool` auxiliary functions. If so, we get the top two values from the stack and add the result of the equality of those two values to the rest of the stack. If the top two elements are the same value, we add `TT` to the rest of the stack. If not, we add `FF` to the rest of the stack.
  - `Le`: For this instruction it was necessary to be sure that the top two elements of the stack are IntValues. So, we first check if the stack has at least 2 elements and if the top 2 elements are both IntValues. To do so, we use the auxiliary function `isInt`. If so, we get the top two values from the stack and add the result of the comparison of those two values to the rest of the stack. If the first element is lower or equal than the second element, we add `TT` to the rest of the stack. If not, we add `FF` to the rest of the stack.
  - `Fetch`: In this instruction, we just need to get the `Value` of a variable from the state. To do so, we use the auxiliary function `findValueFromKey`. Then, we put the value on the top of the stack.
  - `Store`: In this instruction we need to get the top element of the stack and update the state so that the value is bound to the variable. To do so, we use the auxiliary function `removeKey`, and then we add the new pair to the state and return the rest of the stack and the new state.
  - `Branch`: This instruction is a conditional. If the top element of the stack is `TT`, we execute the first instruction of the list of instructions. If it is `FF`, we execute the second instruction of the list of instructions. If it is neither, we return an error.
  - `Noop`: This instruction does nothing. So, we just return the stack and the state.
  - `Loop`: Finally, this instruction is a loop, like a while-loop. It executes `code1` and then calls a `Branch` instruction, where the first instruction is `code2` added to a `Loop` instruction with the same `code1` and `code2`. The second instruction is a `Noop` instruction, so that the program continues if the top element of the stack is `FF`.

- `isBool`: This is an auxiliary function to test if a `Value` is a Boolean Value. To do so, we just check if the `Value` is `TT` or `FF`. If so, we return `True`. If not, we return `False`.

- `findFirst2Int`: This auxiliary function finds the first two `IntValue` of the stack. To to this, we generate an auxiliary list that contains all the elements of the stack different from `TT` and `FF`. Then, we `take` the first two elements of that list, and return them. 

- `removeFirst`: This is an auxiliary function to remove the first occurrence of a value of any type from a list. To do so, we recursively go through the list, and for each element, we check if it is equal to the value we want to remove. If so, we return the list without that element. If not, we add the element to the rest of the list.

- `isInt`: This is an auxiliary function to test if a `Value` is an `IntValue`. If so, we return `True`. If not, we return `False`.
  
- `findValueFromKey`: We needed to find the `Value` of a variable from the state. To do so, we recursively go through the state, and for each tuple, we check if the variable name is equal to the key we want to find. If so, we return the value. If not, we call the function again with the rest of the state. If we reach the end of the state, we return an error.

- `removeKey`: Since we needed to update the state, we needed to remove the old pair of the state, because we can't have two pairs with the same variable name. To do so, we recursively go through the list, and for each tuple, we check if the variable name is equal to the key we want to remove. If so, we return the rest of the list. If not, we add the tuple to the rest of the list.

### Part 1 - Testing

To test the implementation, we used the `testAssembler` function given, and it worked as expected.
When testing with the `testAssembler` function, we noticed that we were not removing the old values on the stack after using them. To do so, we had to change all the functions, so that they return the rest of the stack after using the values. 
Tests: 

```hs
testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
testAssembler [Push (-20),Push (-21), Le] == ("True","")
testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
testAssembler [Push 1,Push 2,And] == "Run-time error"
testAssembler [Tru,Tru,Store "y", Fetch "x",Tru] =="Run-time error"
```

### Part 1 - Conclusion

In this part, we were able to understand properly how the low-level machine works, and how to implement it. We were also able to understand how to use the auxiliary functions to make the code more readable and easier to implement. It was an important part of the work, because it was the base for the next part. It gave us a better understanding of `where` cases and `guard` usage.


### Part 2 - Implementation




### Part 2 - Testing

To test the compiler function, we ran the following tests:

```hs
compile [(Assign "y" (Num 1)), (While (NegE (EqE (Var "x") (Num 1))) [(Assign "y" (MultE (Var "y") (Var "x"))), (Assign "x" (SubE (Var "x") (Num 1)))])] == [Push 1,Store "y",Loop [Push 1,Fetch "x",Equ,Neg] [Fetch "x",Fetch "y",Mult,Store "y",Push 1,Fetch "x",Sub,Store "x"]]
compile [Aex (Num 1), Aex (Var "x")] == [Push 1,Fetch "x"]
```

To test the lexer function, we used the arguments of the tests given, and it worked as expected.


### Part 2 - Conclusion
