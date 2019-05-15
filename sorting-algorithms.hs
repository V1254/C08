{-
    Demonstration of different sorting algorithms , focusing on correctness first and efficiency second.
-}

{-
    Insertion Sort: Take Elements one by one and insert them into the correct positions of the sorted list.
-}


-- Function to insert an element into the correct position in a sorted list.

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs)
 | n <= x    = n:x:xs                    -- if the element is smaller than the head, simply place infront of it.
 | otherwise = x:(insert n xs)           -- otherwise recursivley iterate until you find a position you can put the element in.


-- The actual algorithm making use of the above function 

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs)  = insert x (insertionSort xs)


{-
    Selection sort: find the smallest element and swapp it with the first element, repeat till you get to the end.
-}

-- deletes the first occurence from a list.

delete :: Eq a => a -> [a] -> [a]
delete n [] = []
delete n (x:xs)
 | n == x     = xs
 | otherwise  = x:(delete n xs)


selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = minVal:(selectionSort (delete minVal xs))  -- haskell you cant really swap so you just delete the first occurence here instead.
                    where minVal = minimum xs





