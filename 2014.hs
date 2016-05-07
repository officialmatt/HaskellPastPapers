matches :: (Eq a) => a -> [a] -> [a]
matches _ [] = []
matches x (y:ys)
	| x==y = y:matches x ys
	| otherwise = matches x ys


matches' :: (Eq a) => a -> [a] -> [a]
matches' x ys = [a| a<-ys, a==x]

squareSum :: [Int] -> Int
squareSum xs = foldr (\x acc -> x + acc) 0 (map (\x -> x*x) xs)


insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
	| x<y = x:(y:ys)
	| otherwise = y:insert x ys

isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

sum' :: [Int] -> Int
sum' xs = foldr (\x acc -> x + acc) 0 xs

sumAcc :: Int -> [Int] -> Int 
sumAcc acc [] = acc
sumAcc acc (x:xs) = sumAcc (x+acc) xs

sumInts :: IO Int
sumInts = do
	putStrLn "Please enter your number"
	input <- getLine
	let number = read input
	if number /= 0
	then do
		newNumber <- sumInts
		return (newNumber + number)
	else do
		return 0 