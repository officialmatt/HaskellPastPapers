xor:: Bool -> Bool -> Bool
xor x y 
	| (x == True && y ==True) || (x==False && y == False) = False
	| otherwise = True

xor':: Bool -> Bool -> Bool
xor' x y = x/=y

combi :: (Real a) => [(a,a)] -> [a]
combi xs = [sum[a| x<-xs, let a = fst x, let b = snd x, a > b]] ++ [sum[b| x<-xs, let a = fst x, let b = snd x, a > b]]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort[a | a<-xs, a<=x] ++ [x] ++ qsort[b | b<-xs, b>x]

filterFirst :: (a-> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst p (x:xs) 
	| p x == False = xs
	| otherwise = x: filterFirst p xs

filterLast :: (a-> Bool) -> [a] -> [a]
filterLast _ [] = []
filterLast p (xs) 
	| p (last xs) == False = init xs
	| otherwise = filterLast p (init xs)

backwards :: IO ()
backwards = do
	putStrLn "Enter a piece of text"
	myText <- getLine 
	putStrLn ("The reverse is " ++ show(reverse myText))

mapFuns :: [a->b] -> a -> [b]
mapFuns [] _  = []
mapFuns (x:xs) y = x y: mapFuns xs y

mapFuns' :: [a->b] -> a -> [b]
mapFuns' fs y = foldr(\f acc -> (f y):acc) [] fs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ x [] = x
foldr' f x (y:ys) = f y (foldr' f x ys)

split :: [a] -> ([a],[a])
split xs = ([a | i <- [1..length xs], odd i, let a = xs!!(i-1)] ,[b | i <- [1..length xs], even i, let b = xs!!(i-1)])






