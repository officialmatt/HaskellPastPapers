maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc) 

reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x:acc) [] xs

product' :: (Num a) => [a] -> a
product' xs = foldr (\x acc -> x * acc) 1 xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x acc -> if p x then x: acc else acc) [] xs

head' :: [a] -> a
head' = foldl1 (\x _ -> x) 

last' :: [a] -> a
last' = foldr1 (\_ x -> x)

map' :: (a -> a) -> [a] -> [a]
map' p xs = foldr(\x acc -> p x:acc) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' z ys = foldr(\x acc -> if z == x then True else acc) False ys

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ x [] = x
foldr' f x (y:ys) = f y (foldr' f x ys)

mult :: (Num a) => [a] -> a
mult xs = foldr(\x acc -> x*acc) 1 xs

posList :: [Int] -> [Int]
posList xs = foldr(\x acc -> if x > 0 then x:acc else acc) [] xs

trueList :: [Bool] -> Bool
trueList xs = foldr(\x acc -> if x==True then acc else False) True xs 

evenList :: [Int] -> Bool
evenList xs = foldr(\x acc -> if x `mod` 2 == 0 then acc else False) True xs

maxList :: (Ord a) => [a] -> a
maxList = foldr1 (\x acc -> if x > acc then x else acc)

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b cs = foldr(\x acc -> if (x>a) && (x<b) then x:acc else acc) [] cs

countPositives :: [Int] -> Int
countPositives xs = foldr (\x acc -> x + acc) 0 (map (\x -> 1) (filter (>0) xs))

length' :: [a] -> Int
length' xs = foldr(\x acc -> x + acc) 0 (map(\x -> 1) xs)

myMap :: (a -> a) -> [a] -> [a]
myMap f xs = foldr(\x acc -> f x:acc) [] xs

myLength :: [a] -> Int
myLength xs = foldr (\x acc -> x + acc) 0 ((foldr(\x acc -> 1:acc) [] xs))










