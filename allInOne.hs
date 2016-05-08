ones :: a -> Int
ones _ = 1

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' xs = foldr (\x acc -> x + acc) 0 (map(\x -> ones x) xs)

mod' :: Int -> Int -> Int
mod' x y
	| x - y < y = x-y
	| otherwise = mod' (x-y) y 

divides :: Int -> Int -> Bool
divides x y
	| mod y x == 0 = True
	| otherwise = False

strictFactors :: Int -> [Int]
strictFactors x = [a| a <- [2..(x-1)], divides a x == True]

primesUpTo :: Int -> [Int]
primesUpTo x = [a | a <-[2..(x)], strictFactors a == []]

primeList :: IO ()
primeList = do
	putStrLn "Enter a whole number"
	myNumber <- getLine
	let number = read myNumber
	putStrLn  ("The primes are " ++ show(primesUpTo number))

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) 
	| x == y = True
	| otherwise = elem' x ys

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' z ys = foldr(\x acc -> if x == z then True else acc) False ys

alphanums :: [Char] -> [Char]
alphanums xs = [a | a <- xs, a/= '.' , a/= ';' ,a/= ',' ,a/= '\'' , a/= ' ' ]

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ x [] = x
foldr' f x (y:ys) = f y (foldr' f x ys) 

mySum :: (Num a) => [a] -> a
mySum xs = foldr (\x acc -> x + acc) 0 (map(\x -> x*x*x) xs)

sameHead :: (Eq a) => [a] -> [a] -> Bool
sameHead _ [] = False
sameHead [] _ = False
sameHead (x:xs) (y:ys) 
	| x == y = True
	| otherwise = False

biggerThan :: (Ord a) => a -> [a] -> [a]
biggerThan _ [] = []
biggerThan x (y:ys)
	| y> x = y:(biggerThan x ys)
	| otherwise = biggerThan x ys

sameList :: (Eq a) => [a] -> [a] -> Bool
sameList [] [] = True
sameList _ [] = False
sameList [] _ = False
sameList (x:xs) (y:ys)
	| x==y = sameList xs ys
	| otherwise = False

wonkeyLen :: IO ()
wonkeyLen = do
	putStrLn "Enter your text"
	myText <- getLine
	putStrLn ("The wonky length is " ++ show(((length myText)+1)))

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ f a b | (a,b) <- zip xs ys]

absDiff :: Float -> Float -> Float
absDiff x y = abs (x-y)

nearEq :: Float -> Float -> Float -> Bool
nearEq x y z 
	| absDiff y z < x = True
	| otherwise = False

probDist :: [Float] -> Bool
probDist xs = nearEq 0.0001 1 (sum xs)

rv :: [a] -> [Float] -> [(a,Float)]
rv xs ys 
	| (probDist ys) && (length xs == length ys) = zip xs ys
	| otherwise = []


matches :: (Eq a) => a -> [a] -> [a]
matches _ [] = []
matches x (y:ys)
	| x == y = y:matches x ys
	| otherwise = matches x ys 

matches' :: (Eq a) => a -> [a] -> [a]
matches' x ys = [ a | a <- ys, a == x]

insert' :: (Ord a) => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys)
	| x < y = x:(y:ys)
	| otherwise = y:(insert' x ys) 

isort' :: (Ord a) => [a] -> [a]
isort' [] = []
isort' (x:xs) = insert' x (isort' xs)

--data Set a = Set [a]

--mapSet :: (Ord a, Ord b) => (a-> b) -> Set a -> Set B
--mapSet f (Set xs) = makeSet (map f xs) 

--filterSet :: (Ord a) => (a -> Bool) -> Set a -> Set a
--filterSet f (Set xs) = makeSet (filter f xs)

--memSet :: (Ord a) => a -> Set a -> Bool
--memSet a (Set []) = False
--memSet a (Set (x:xs)) 
--	| a == x = True
--	| a < x = False
--	| otherwise = memSet a (Set xs

sum' :: [Int] -> Int
sum' xs = foldr(\x acc -> x + acc) 0 xs

sumAcc :: Int -> [Int] -> Int
sumAcc x [] = x
sumAcc x (y:ys) = sumAcc (x+y) ys

sumInts :: IO Int
sumInts = do
	putStrLn "Enter a number"
	number <- getLine
	let myNumber = read number
	if myNumber /= 0 then 
		do
			newNumber <- sumInts
			return (newNumber + myNumber)
	else
		do
			return 0


xor :: Bool -> Bool -> Bool
xor x y = x /= y

combi :: (Real a) => [(a,a)] -> [a]
combi xs =  [sum[a| i <- xs, let a = fst i, let b = snd i, a>b]] ++ [sum[ b | i <- xs, let a = fst i, let b = snd i, a>b  ]]

zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith f xs ys = [ f a b | (a,b) <- zip xs ys]

qSort :: (Ord a) => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort[a | a<-(xs) , a <= x] ++ [x] ++  qSort [b | b<-(xs) , b > x]



--curry :: ((a,b) ,c) -> a -> b -> c
--curry f a b = f (a,b)

--uncurry :: a -> b -> c -> ((a,b),c)
--f (a,b) = f a b 


filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst p (x:xs)
	| p x == False =  xs
	| otherwise = x:filterFirst p (xs)

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast _ [] = []
filterLast p xs 
	| p (last xs) == False = init xs
	| otherwise = filterLast p (init xs) ++ [last xs]

backwards :: IO ()
backwards = do
	putStrLn "Enter some text"
	myText <- getLine
	putStrLn ("The reverse is " ++ show(reverse myText))

mapFuns :: [a -> b] -> a -> [b]
mapFuns [] _ = []
mapFuns (f:fs) x = f x : mapFuns fs x

mapFuns' :: [a -> b] -> a -> [b]
mapFuns' fs y = foldr(\x acc -> x y : acc) [] fs

foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' _ x [] = x
foldr'' p x (y:ys) = p y (foldr'' p x ys)

split:: [a] -> ([a],[a])
split xs = ([a | i <-[0..((length xs)-1)], even i,  let a = xs!!i] ,[b| i <-[0..((length xs)-1)], odd i,  let b = xs!!i])








	