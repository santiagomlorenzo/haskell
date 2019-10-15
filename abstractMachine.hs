data Expression = Value Int | Addition Expression Expression
type ControlStacks = [Operation]
data Operation = EVAL Expression | ADD Int

value :: Expression -> Int
value (Value value) = value
value (Addition expression1 expression2) = value expression1 + value expression2

eval :: Expression -> ControlStacks -> Int
eval (Value value) controlStacks                      = execute controlStacks value
eval (Addition expression1 expression2) controlStacks = eval expression1 (EVAL expression2 : controlStacks)

execute :: ControlStacks -> Int -> Int
execute [] value = value

execute (EVAL expression : remainingControlStacks) value = 
  eval expression (ADD value : remainingControlStacks)

execute (ADD operationValue : remainingControlStacks) currentValue = 
  execute remainingControlStacks (operationValue+currentValue)

withControlStackValue :: Expression -> Int
withControlStackValue expression = eval expression []