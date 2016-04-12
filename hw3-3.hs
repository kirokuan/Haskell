data Proposition = Var String
                    | F
                    | T
                    | Not Proposition
                    | Proposition :|: Proposition
                    | Proposition :&: Proposition
                    deriving (Eq, Ord, Show)

isNorm :: Proposition -> Bool
isNorm T =True
isNorm F =True
isNorm (Var _) =True

isNorm (Not (Var x))=True
isNorm (Not p) =False
isNorm (a :|: b) = isNorm a && isNorm b 
isNorm (a :&: b) = isNorm a && isNorm b 

norm :: Proposition -> Proposition
norm a@(Not p) | isNorm a = a
               | otherwise =  norm (rev p)
norm (a :|: b) = norm a :|: norm b
norm (a :&: b) = norm a :&: norm b
norm p=p


rev :: Proposition -> Proposition
rev T = F
rev F = T
rev (Not p)= p
rev (p :|: q) = norm (Not p) :&: norm (Not q)
rev (p :&: q) = norm (Not p) :|: norm (Not q)