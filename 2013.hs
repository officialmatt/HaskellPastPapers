import Data.Char

sameHead :: (Eq a) => [a] -> [a] -> Bool
sameHead _ [] = False
sameHead [] _ = False
sameHead (x:xs) (y:ys)
	| x == y = True
	| otherwise = False 

biggerThan :: (Ord a) => a -> [a] -> [a]
biggerThan _ [] = []
biggerThan x (y:ys)
	| y > x = y:biggerThan x ys
	| otherwise = biggerThan x ys

sameList :: (Eq a) => [a] -> [a] -> Bool
sameList [] [] = True 
sameList _ [] = False
sameList [] _ = False
sameList (x:xs) (y:ys)
	| x==y = sameList xs ys
	| otherwise = False

sumPosSqr :: [Int] -> Int
sumPosSqr xs = foldr (+) 0 (map (^2) (filter (>0) xs))

sumPosSqr' :: [Int] -> Int
sumPosSqr' xs = foldr (\x acc -> x + acc) 0 (map (\x -> x*x) (filter (>0) xs))

wonkeyLen :: IO ()
wonkeyLen =  do
		putStrLn "Enter your string"
		myString <- getLine
		--let myString1 = read myString 
		--let myLength = length myString1 
		putStrLn ("The wonky length is " ++ show ((length myString)+1))

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ f a b | a <- xs, b <- ys]

absDiff :: Float -> Float -> Float
absDiff x y 
	| (y - x) > 0 = y-x
	| otherwise = (-1) * (y-x)

nearEq :: Float -> Float -> Float -> Bool
nearEq x y z 
	| z < absDiff y z = False
	| otherwise = True

probsDist :: [Float] -> Bool
probsDist xs = nearEq 0.0002 (sum xs) 1.0

rv :: (Ord a) => [a] -> [Float] -> [(a,Float)]
rv xs ys 
	| (length xs == length ys) && probsDist ys == True = zip xs ys
	| otherwise = []






