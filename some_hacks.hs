-- Factoriel recursive function
fact:: Int -> Int
fact x 
  |x==0 = 1
  |x>0 = fact(x-1) * x
  

-- Checks if the given number is prime
isPrime:: Int-> Int-> Bool
isPrime x y
    | (x`mod`y) == 0  = False
    | (y == x`div`2)  = True
    | otherwise       = x`isPrime`(y+1)

-- Checks if the element is present in the list 
isPresent :: Int -> [Int] -> Bool
isPresent x (y:xs) 
    | xs == []  = False
    | x==y      = True
    | otherwise = isPresent x xs


-- Gets the digits of a given number as a list elements
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

-- Multiplies the given numbers
multiply :: Int->Int->Int
multiply x y = x*y

-- Evaluates the passed function with all the elements of the list
attack :: (Int -> Int) -> [Int] -> [Int] 
attack f xs = [f x | x<-xs]

data NTree = NilT | Node Int NTree NTree

-- A method to search if a given node is present in binary tree
occurs :: NTree -> Int -> Int
occurs NilT p = 0
occurs (Node n t1 t2) p
 | n==p = 1 + occurs t1 p + occurs t2 p
 | otherwise = occurs t1 p + occurs t2 p
 

main :: IO()
main = do 
        print $ isPrime 5 2
        print $ (isPresent 21 [1, 2, 4, 5])
        print $ (attack (multiply 2) [1, 2, 4, 7])
        print $ (powEven [1, 2, 3 ,4])
        print $ (occurs (Node 17 (Node 14 NilT NilT) (Node 20 NilT NilT)) 21)
        
