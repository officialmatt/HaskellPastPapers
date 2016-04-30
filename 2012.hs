import Data.Char
import Test.QuickCheck
 
ones :: a -> Int 
ones _ = 1


myLength :: [a] -> Int
myLength xs = foldr (+) 0 (map (\x -> 1) xs) 

mod' :: Int -> Int -> Int
mod' x y 
	| x-y < y = y
	| otherwise = mod' (x-y) y

divides :: Int -> Int -> Bool
divides x y 
	| mod x y == 0 = True
	| otherwise = False

strictFactors :: Int -> [Int]
strictFactors x = [a | a<-[2..(x-1)], divides x a]

primesUpTo :: Int -> [Int]
primesUpTo x = [a | a<-[2..(x)], strictFactors a ==[]]

primesList :: IO () --Used Davids here 
primesList = do
	putStrLn "Enter the whole number"
	number <- getLine
	let num = read number 
	putStrLn ("The primes up to " ++ number ++ " are " ++ show(primesUpTo num))

elem' :: (Eq a) => a -> [a] -> Bool
elem' x ys 
	| length (filter (==x) ys) >= 1 = True
	| otherwise = False

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' _ [] = False
elem'' y (x:xs)
	| y == x = True
	| otherwise = elem'' y xs

alphanums :: String -> String 
alphanums xs = [a | a <- xs, a/='.', a/=',', a/=';' , a/= ' ' , a/= '\'']

mySum :: (Num a) => [a] -> a
mySum xs = foldr (+) 0 (map (^3) xs)