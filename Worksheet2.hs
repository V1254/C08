
--------------------------------------------------------------------
-- CO 2008  Functional Programming
-- Created: February 2019, University of Leicester, UK
-- handin 17.00 hr on Sunday 17st February (assessed) 
--------------------------------------------------------------------
-- Student Name: Mohamed Abdulwahid Sharif-Farah
-- Student Number: 179029141
--------------------------------------------------------------------
--
-- Please don't hand in buggy solutions. That makes the marking harder.
-- Points may be deducted if your solution does not compile properly...
-- use a good looking layout

module Worksheet2 where
import Data.Char

----------------------------------------------------------------------
-- Exercise 1: A phone book
---------------------------------------------------------------------

type Name = String
type PhoneNumber = Int
type Person  = (Name, PhoneNumber)
type PhoneBook = [Person]

-- Part a)

add :: Person -> PhoneBook -> PhoneBook
add person phoneBook = person : phoneBook

-- Part b)

delete  :: Name -> PhoneBook -> PhoneBook
delete name phoneBook = [x| x <- phoneBook, (fst x) /= name]

--  Part c)

find  :: Name -> PhoneBook -> [PhoneNumber]
find  name phoneBook = [snd k| k <- phoneBook , (fst k) == name] 

--  Part d)

update :: Name ->  PhoneNumber -> PhoneNumber-> PhoneBook -> PhoneBook
update name old new phoneBook = map(\x -> if x == (name,old) then (name,new) else x) phoneBook

-----------------------------------------------------------------
-- Exercise 2:  Customers of a Bank
-----------------------------------------------------------------

type NI = Int
type Age = Int
type Balance = Float
type Customer  = (NI,Age, Balance)
type Bank =  [Customer]

-- Part a)

retired :: Customer -> Bool
retired (nI,age,balance) = age >= 67

-- Part b)

deposit :: Customer -> Float -> Customer
deposit (nI,age,balance) amount = (nI,age,(balance+amount))

-- Part c)

withdraw :: Customer -> Float -> Customer
withdraw (nI,age,balance) amount
    | balance >= amount = (nI,age,balance-amount)
    | otherwise = (nI,age,balance)

-- Part d)

credit :: Bank -> [Customer]
credit bank = [(nI,age,balance)| (nI,age,balance) <- bank,  balance >= 0]
-----------------------------------------------------------------
-- Exercise 3: cubeOdds
-----------------------------------------------------------------

-- True if an Int is odd.
isOdd :: Int -> Bool
isOdd n = n `mod` 2 /= 0

cubeOdds :: [Int]-> [Int]
cubeOdds xs = [val^3| val <- xs, isOdd val]

cubeOdds2 :: [Int]-> [Int]
cubeOdds2 xs = map(\x -> x^3) oddList where 
                                        oddList = filter odd xs


-----------------------------------------------------------------
-- Exercise 4. addIndex
-----------------------------------------------------------------

addIndex :: [Int] -> [(Int,Int)]
addIndex xs = zip [1..(length xs)] xs