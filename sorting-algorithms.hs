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


{-
    MergeSort: Merges sorted lists by repeatededly dividing the list until one elemnt is left (hence sorted)
-}

-- first a function to merge two sorted lists.

merge :: Ord a => [a] -> [a] -> [a]
merge [] k = k
merge k [] = k
merge (a:as) (b:bs)
 | a <= b = a:(merge as (b:bs))
 | otherwise = b:(merge (a:as) bs)

-- now a function to halve a list.
halve :: [a] -> ([a],[a])
halve xs = (take n xs , drop n xs)
            where n = (length xs) `div` 2


-- The algorithm itself.
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge (mergeSort firstHalf) (mergeSort secondHalf)
                        where 
                             firstHalf  = fst halves
                             secondHalf = snd halves
                             halves = halve list

{-
    QuickSort: select a pivot, place all element that are smaller before the pivot and bigger elements after the pivot,
        do this recursively.
-}

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [a] = [a]
quickSort (x:xs) = quickSort (ls) ++ [x] ++ quickSort (us)
                    where ls = filter (<= x) xs
                          us = filter (> x) xs 
                          


{-
    BubbleSort : take two elements sort em, continue down the list and repeat till no swaps are performed anymore.
-}


-- places the first element of a list into the correct position by repeatededly comparing it to the second element until it is in the correct positon.

bubbleSwap :: Ord a => [a] -> [a]
bubbleSwap [] = []
bubbleSwap [a] = [a]
bubbleSwap (x:y:z)
 | x < y = x:(bubbleSwap (y:z))
 | otherwise = y:(bubbleSwap (x:z))


bubbleSort :: Ord a => [a] -> [a]
bubbleSort list 
 | (bubbleSwap list) == list = list                  -- stop the algorithm if the result of the swapped list is the same as the input.
 | otherwise = bubbleSort (bubbleSwap list)          -- continue the algorithm if a swap was made hence the list and the slightly sorted list are not the same.
                    






