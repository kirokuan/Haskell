countNeg :: [Int] ->  Int
countNeg [] =0
countNeg (x:xs) | x>=0 = countNeg xs
                | x<0 = 1+(countNeg xs)
              
raise :: Int ->  Int ->  Int
raise x 1 =x
raise x n =x*(raise x (n-1))

pascal :: Int -> [Int] 
pascal 1 =[1]
pascal n = 
          let  last1=(pascal (n-1))
          in (myZip (last1++[0]) ([0]++last1))

myZip :: [Int]->[Int]->[Int]
myZip (x:xs) (y:ys) =(x+y):(myZip xs ys)
myZip _ _ =[]

subsets :: [Int] -> [[Int]]
subsets [] =  [ [] ]
subsets (x:xs) =  let lastOne=(subsets xs)
                 in lastOne++ (map (++[x]) lastOne)

q1f1a :: [Int] -> [Int]
q1f1a (x:xs) | 2< x && x<11 = (x*3):  q1f1a xs
             | otherwise =q1f1a xs
q1f1a _=[]

q1f1b :: [Int] -> [Int]
q1f1b xs = [ v*3 | v <- xs,2<v && v<11]

compre :: [a]->(a->b)->(a->Bool)->[b]
compre [] f p =[]
compre (x:xs) f p | p x = (f x): (compre xs f p)
                  | otherwise = compre xs f p                 

maxl :: (Ord a) => [a] -> a
maxl []= error("empty list")
maxl (x:xs) = (foldr max x xs)

member :: (Eq a) => [a] -> a -> Bool
member xs s = foldr (||) False (map (==s) xs) 

occurrences :: [Char] -> [(Char,Int)]
occurrences []=[]
occurrences (x:xs) =  (x,(elemOcc x xs)):occurrences (filter (/=x) xs)

elemOcc :: Char ->[Char] -> Int
elemOcc c text = length (filter (==c) text)+1
-- map (+1) [1,2,3] =[2,3,4]
--foldr (+) 1 [1]
myMap :: (a->b)->[a]->[b]
myMap f [] = []
myMap f x = foldr (\y ys -> (f y):ys) [] x
