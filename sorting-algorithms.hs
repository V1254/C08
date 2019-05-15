{-
    Demonstration of different sorting algorithms , focusing on correctness first and efficiency second.
-}

{-
    Insertion Sort: Insert an element x into a sorted list.
-}


-- Function to insert an element into the correct position in a sorted list.

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs)
 | n <= x    = n:x:xs                    -- if the element is smaller than the head, simply place infront of it.
 | otherwise = x:(insert n xs)           -- otherwise recursivley iterate until you find a position you can put the element in.


insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs)  = insert x (insertionSort xs)
