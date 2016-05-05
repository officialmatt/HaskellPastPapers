matches :: (Eq a) => a -> [a] -> [a]
matches _ [] = []
matches x (y:ys)
	| x==y = y:matches x ys
	| otherwise = matches x ys


matches' :: (Eq a) => a -> [a] -> [a]
matches' x ys = [a| a<-ys, a==x]

squareSum :: [Int] -> Int
squareSum xs = foldr (\x acc -> x + acc) 0 (map (\x -> x*x) xs)
