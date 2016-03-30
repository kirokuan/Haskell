countNeg :: [Int] ->  Int
countNeg [] =0
countNeg x:xs |x>=0 = countNeg xs
              |x<0 = 1+(countNeg xs)
              
raise :: Int ->  Int ->  Int
raise x 1 =x
raise x n =x*(raise x (n-1))

Pascal :: Int->[Int] 
Pascal 1 =[1]
Pascal n =MyZip (Pascal (n-1):0) (0:(Pascal (n-1)))

MyZip :: [Int]->[Int]->[Int]
MyZip x:xs y:ys =(x+y):MyZip xs ys
MyZip _ _ =[]


q1f1a :: [Int] -> [Int]
q1f1a (x:xs) | x >= 3 and x<=10 = x*3 : q1f1a xs
q1f1a _=[]


q1f1b :: [Int] -> [Int]
q1f1b xs ={v*3 for v in xs if v < 3 || v>10}
    

subsets :: [Int] -> [[Int]]
subsets [] =  [ [] ]
subsets x:xs =  (subsets xs)++(x:(subsets xs))

