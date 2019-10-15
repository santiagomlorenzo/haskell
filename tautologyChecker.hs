data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop

type Assoc k v = [(k,v)]

type Subst = Assoc Char Bool

p1 :: Prop 
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

find :: Eq k => k -> Assoc k v -> v
find key assocTable = head [value | (key',value) <- assocTable, key == key']

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
-- rmdups (current:rest) = current : rmdups (filter (/= current) rest) [my version]
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval subsTable (Var key) = find key subsTable
eval subsTable (Not prop) = not (eval subsTable prop)
eval subsTable (And prop1 prop2) = eval subsTable prop1 && eval subsTable prop2
eval subsTable (Imply prop1 prop2) = eval subsTable prop1 <= eval subsTable prop2

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not prop) = vars prop
vars (And prop1 prop2) = vars prop1 ++ vars prop2
vars (Imply prop1 prop2) = vars prop1 ++ vars prop2

booleanCartesianProduct :: Int -> [[Bool]]
booleanCartesianProduct 0 = [[]]
booleanCartesianProduct size = map (False:) bss ++ map (True:) bss
  where bss = booleanCartesianProduct(size - 1)

substs :: Prop -> [Subst]
substs p = map (zip propositionVariables) truthTable
  where 
    propositionVariables = rmdups (vars p)
    truthTable = booleanCartesianProduct (length propositionVariables)

isTaut :: Prop -> Bool
isTaut proposition = and propositionValues
  where
    subsTable = substs proposition
    propositionValues = map (\entry -> eval entry proposition) subsTable