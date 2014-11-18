-- Question 1 
-- Write an expression that evaluates to a list of the even numbers that are a multiple of 7 and less than 200

ques1 :: [Int] 
ques1  = [x | x <- [2,4..200], x `mod` 7 == 0]

-- Question 2 
-- Write a function that takes two integers and evaluates to a list of integers between the first integer and the second integer, including the first and second integers

ques2 :: Int -> Int -> [Int]
ques2 a b  | b < a = [b..a]
					 | otherwise = [a..b]

-- Question 3
-- Write a fucntion, diff, that takes two lists and returns a list that is all the elements of the first list that are not in teh second list,, e.g diff[1,2,3,4,5,6,7][2,4,6,8] evaluates to [1,3,5,7]

diff :: [Int] -> [Int] -> [Int]
diff a [] = a 
diff [] _ = []
diff (a:as) b =  if elem a b
								 then diff as b
								 else a:diff as b

-- Question 4 -- 
-- Write a data type for a binary tree

data BinTree t = Empty | Root t (BinTree t) (BinTree t)
								 deriving (Eq, Ord, Show)
-- Question 5 --
-- Write a function that converts a binary tree into a list by doing a postorder traversal of the binary tree. Remember a postorder traversal visits the left child then the right and then parent node,

leaf :: x -> BinTree x
leaf x = Root x Empty Empty
myTree = Root 3 (Root 7 (Empty) (leaf 2)) (leaf 9)

------------------- Some other bin tree stuff for revision -----------------

-- adding an element

addNode :: Ord x => x -> BinTree x -> BinTree x
addNode x Empty = leaf x
addNode a (Root x l r) | a < x = Root (addNode a l) r
											 | otherwise = Root x l (addNode a r)



-- Actual Function.


postorder :: BinTree x-> [x]
postorder Empty = []
postorder (Root x l r ) = postorder l ++ postorder r ++ [x]



