-- My List Functions - Question 1 --

-- myAppend

myAppend :: [a] -> [a] -> [a]
myAppend [] xs = xs
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
myConcat (x:xs) = myAppend x (myConcat(xs))

-- mySum

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

-- myProduct

myProduct :: Num a => [a] -> a
myProduct []= error "NO"
myProduct [x] = x
myProduct (x:xs) = x * (myProduct xs)

-- myMaximum

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "NO"
myMaximum [x] = x
myMaximum (x:xs) | x > myMaximum xs = x
								 | otherwise = myMaximum xs

-- myMinimum 

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "Empty pal"
myMinimum [x] = x
myMinimum (x:xs) | x < myMinimum xs = x
								 | otherwise = myMinimum xs

-- myElem 

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) | x == y = True
								| otherwise = myElem x ys
								
-- myDelete

myDelete ::  Eq a => a -> [a] -> [a]
myDelete x [] = []
myDelete x (y:ys) | x == y = ys
									| otherwise = y:myDelete x ys


-- Question 2 --

-- myUnion

myUnion :: Eq a =>[a] -> [a] -> [a] 
myUnion x [] = x
myUnion x (y:ys) | myElem y x = myUnion x ys
							   | otherwise = myUnion (myAppend x[y]) ys

-- myIntersect

myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] x = []
myIntersect (x:xs) y | myElem x y = x:myIntersect xs y 
										 | otherwise = myIntersect xs y












								 
