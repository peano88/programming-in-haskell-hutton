-- Tautology checker ---
data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  | Or Prop Prop
  | LogEq Prop Prop

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var c) = find c s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (LogEq p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (LogEq p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = []
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n -1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs) --more efficient : x: filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- Abstract Machine ---

data Expr = Val Int | Add Expr Expr | Mult Expr Expr

value :: Expr -> Int
value (Val x) = x
value (Add e f) = value e + value f
value (Mult e f) = value e * value f

data Op = EVAL Expr | ADD Int | MULT Int

type Cont = [Op]

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL x : c) n = evalM x (ADD n : c)
exec (ADD x : c) n = exec c (x + n)
exec (MULT x : c) n = exec c (x * n)

evalM :: Expr -> Cont -> Int
evalM (Val x) c = exec c x
evalM (Add x y) c = evalM x (ADD (evalM y []) : c) -- in this case the exec with EVAL is no longer needed
evalM (Mult x y) c = evalM x (MULT (evalM y []) : c)

valueM :: Expr -> Int
valueM e = evalM e []