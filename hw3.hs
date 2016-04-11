
data Tree a = Empty | Node a (Tree a) (Tree a) 
                                   	deriving Show
bfs :: Tree a -> [a]
bfs x = traverse2 [x]

traverse2 :: [Tree a] -> [a]  
traverse2 [] = []
traverse2 ts = rootlabels ++ (traverse2 childrenLeft)++ (traverse2 childrenRight)
    where  rootlabels = [ a | (Node a l r)<-ts ] 
           childrenLeft = [l | (Node a l r)<-ts ]  
           childrenRight = [r | (Node a l r)<-ts ] 

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
isNorm (Not(a :|: b))=False
isNorm (Not(a :&: b))=False
isNorm (Not a)=True
isNorm (a :|: b) = isNorm a && isNorm b 
isNorm (a :&: b) = isNorm a && isNorm b 

norm :: Proposition -> Proposition
norm (Not T) = F
norm (Not F) = T
norm (Not (Not b)) = b
norm (Not (p :|: q)) = Not p :&: Not q
norm (Not (p :&: q)) = Not p :|: Not q

data Edit = Change Char | Copy | Delete | Insert Char 
                                         deriving (Eq, Show)
transform:: String -> String -> [ Edit ]
transform st [] = replicate (length st) Delete
transform [] st = map Insert st
transform (a:x) (b:y)
    | a==b = Copy : transform x y
    | otherwise = best [ Delete : (transform x (b:y)) ,
                         Insert b : (transform (a:x) y) ,
                         Change b : (transform x y) ]
best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
  | cost x <= cost b = x
  | otherwise = b
       where
             b = best xs

cost :: [Edit] -> Int
cost = length . filter (/=Copy)
