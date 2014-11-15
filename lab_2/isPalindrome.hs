reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverseList x 