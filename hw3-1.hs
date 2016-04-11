--type String = [Char]
type Name = String
type Env = [(Name, Int)]
data Expr =  Var Name | Lit Int | Expr :+: Expr | Expr :*: Expr
             deriving (Eq, Show)

eval :: Env -> Expr -> Int
eval e (Var x) = lookUp e x
eval e (Lit x) =  x
eval e (p :+: q) = eval e p + eval e q
eval e (p :*: q) = eval e p * eval e q


lookUp :: Eq a => [(a,b)] -> a -> b
lookUp xys x = that [ y | (x1, y) <- xys, x == x1 ]
                where that[x]=x