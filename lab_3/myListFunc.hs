-- My List Functions - Question 1 --

-- myAppend

myAppend :: [a] -> [a] -> [a]
myAppend [] xs = xs
--myAppend [] = []
myAppend (y:ys) xs = y:(myAppend ys xs)

-- myHead 

myHead :: [a] -> a
myHead [] = error "Error, no head cos no list bruh"
myHead (x:xs) = x

-- myTail

myTail :: [a] -> [a]
myTail [] = error "Error, no tail in an empty list!!!"
myTail (x:xs) = xs

-- myLast

myLast :: [a] -> a 
myLast [] = error "Yeet"
myLast [x] = x
myLast (x:xs) = myLast(xs)

-- myInit

myInit :: [a] -> [a]
-- myInit [] = []
myInit [x] = []
myInit (x:xs) = x:myInit(xs)

-- myLength

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)

-- myReverse

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- myConcat 

myConcat :: [[a]] -> [a] 
myConcat [] = []
myConcat (x:xs) = myAppend(xs)


























