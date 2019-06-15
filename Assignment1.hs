myLast :: [a]->a
mylast x = last x

myReverse :: [a]->[a]
myReverse x = reverse x

ispalindrome :: Eq a => [a]->Bool
ispalindrome  x = x == myreverse x

compress :: (Eq a) => [a] -> [a]
compress a
 |(xs==[]) = [x]
 | x== head xs = [] ++ compress xs
 | otherwise = [x] ++ compress xs

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x,x] ++ duplicate xs

rotate :: [a] -> Int-> [a]
rotate x n
 | n==0 = x
 | otherwise = (rotate (tail x) (n-1)) ++ [head x]

insertAt :: a -> [a] -> Int -> [a]
insertAt x y n
 | n==1 = [x] ++ y
 | otherwise = [head y] ++ insertAt x (tail y) (n-1)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [xs !! index : x | index <- [0..(length xs)-1] , x <- combinations (n - 1) (drop (index + 1) xs) ]


isprime :: Int -> Bool
isprime a = null[x|x<-[2,3..a-1],x `mod` k == 0]

 