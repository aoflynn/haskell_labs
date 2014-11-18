-- 2010 Lab exam

-- Question 1
-- List of odd numbers from 1 - 99

listNum = [x| x <- [1,3..99]]

-- Question 2
-- List even numbers not evenly divisible by 3 and whhose sum is less than 1000

whileSum :: [Int] -> Int -> Int -> [Int]
whileSum (x:xs) sum target | x + sum >= target =[]
													 | otherwise = x:(whileSum xs (sum + x) target)
whileSumAns = whileSum[x | x <- [2,4..], mod x 3 /=0] 0 1000

-- Question 3 
-- 2013 question 

diff :: [Int] -> [Int] -> [Int]
diff a [] = a
diff [] _ = []
diff (x:xs) y | elem x y = diff xs y
							| otherwise = x:diff xs y

-- Question 4
-- 2013 question

data BinTree x = Empty | Root x (BinTree x) (BinTree x)
								deriving (Eq, Ord, Show)
								

-- Question 5
-- Function to add elements to a bin tree
leaf x = Root x Empty Empty
addNode :: Ord a => a -> BinTree a -> BinTree a
addNode n Empty = leaf n
addNode n (Root x l r) | n < x = Root x (addNode n l) r
											 | otherwise = Root x l (addNode n r)

-- Other binary tree questions

-- MakeTree
makeTree :: Ord a => [a] -> BinTree a
makeTree [] = Empty
makeTree [x] = leaf x
makeTree (x:xs) = addNode x (makeTree xs)
