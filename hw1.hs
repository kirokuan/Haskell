  power :: Int -> Int -> Int
  power n k | k < 0 = error "power: negative argument"
  power n 0 = 1
  power n k = n * power n (k-1)

  power1:: Int -> Int -> Int
  power1 n k | k < 0 = error "power: negative argument"
  power1 n 0 = 1
  power1 n k =product (take k (repeat n))

  power2:: Int -> Int -> Int
  power2 n k | k < 0 = error "power: negative argument"
  power2 n 0 = 1
  power2 n 2 = n*n
  power2 n k | even k =power2 (power2 n 2) (k `div` 2)
             | odd k  =n*(power2 n (k-1))

  myButLast :: [a] -> a
  myButLast (x:xs)| length xs >1 =myButLast xs
                  | length xs ==1 =x

  rev2 :: [a] -> [a]
  rev2 s@(x:xs:[]) = reverse s
  rev2 s@(x:xs:_) = s 

  palindrome :: [Int] -> Bool
  palindrome [] = True
  palindrome (x:[]) = True
  palindrome (x:xs) = x == last xs && palindrome (drop (length xs -1) xs)
  
  removeOnce:: Int->[Int]->[Int]
  removeOnce k []= []
  removeOnce k (x:xs) | x==k = xs
                       | x/=k = x:(removeOnce k xs)


  isPermutation :: [Int] -> [Int] -> Bool
  isPermutation [] [] = True
  isPermutation [] (x:_) =False
  isPermutation (x:xs) s = isPermutation xs (removeOnce x s)
  
  fib n = tailFib n 0 1
  
  tailFib :: Int->Int->Int->Int
  tailFib 0 b c =c
  tailFib a b c =tailFib (a-1) c (b+c)
  
  upto :: Int->Int->[Int]
  upto a b | a>b =[]
  upto a b =tailUpto a b []
  
  tailUpto :: Int->Int->[Int]->[Int]
  tailUpto a b x | a==b =b:x
  tailUpto a b x =tailUpto a (b-1) (b:x) 
  