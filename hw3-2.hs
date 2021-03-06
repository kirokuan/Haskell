data Tree a = Empty | Node a (Tree a) (Tree a) 
                                   	deriving Show
bfs :: Tree a -> [a]
bfs x = traverse2 [x]

traverse' :: [Tree a] -> [a]  
traverse' [] = []
traverse' ts = rootlabels ++ (traverse2 childrenLeft)++ (traverse2 childrenRight)
    where  rootlabels = [ a | (Node a l r)<-ts ] 
           childrenLeft = [l | (Node a l r)<-ts ]  
           childrenRight = [r | (Node a l r)<-ts ] 

