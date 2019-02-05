--------------------------------------------------------------------
-- CO 2008  Functional Programming                           
-- Created: February 2018, University of Leicester, UK

-- Handindate  18.00 on Sunday 10/2/2019
-------------------------------------------------------------------- 
--
-- DON'T FORGET TO FILL IN NAME AND STUDENT NUMBER.
--
-------------------------------------------------------------------          
-- Student Name: Mohamed Abdulwahid Sharif-Farah 
-- Student Number: 179029141
--------------------------------------------------------------------

--
--use GHCI
--

--

module Worksheet1 where 
import Data.Char

-----------------------------------------------------------------
-- Exercise 1 
-----------------------------------------------------------------

type Verb  = String

-- pastTense :: Verb -> Verb
-- pastTense  v = ?

----------------------------------------------------------------------
-- Exercise 2 
----------------------------------------------------------------------

type Mass = Float
type Height = Float
type BMI = Float

--bmi :: Mass -> Height -> BMI
bmi :: Mass -> Height -> BMI
bmi m h = m / (h^2)

----------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------

type NumberOfCars = Int
type DailyCost  = Float

cost :: NumberOfCars  -> DailyCost
cost n
    | n >=0 && n <=500 = (5 * fromIntegral n) + 1000
    | otherwise = (10 * fromIntegral n) + 450
  
---------------------------------------------------------------------
-- Exercise 4. 
---------------------------------------------------------------------

-- This year is 2018.
-- if you are born in 10 (abbreviation for 2010)
-- your age is 8.

-- if however you are born in 65 (the that must be abbreviation for 1965)
-- your age is 2018 - 1965 = 53

--age :: Year -> Int
--age ?

---------------------------------------------------------------------
-- Exercise 5.  Pounds Euros
---------------------------------------------------------------------

type Euros = Float
type Pounds = Float

eurocurrency = 1.14 :: Float

--p2e  :: Pounds -> Euros
--p2e ?

--e2p :: Euros -> Pounds
--e2p ?

---------------------------------------------------------------------
-- Exercise 6.  Pounds Euros refined
---------------------------------------------------------------------

--prettyprintEuro :: Euro -> String

--prettyprintPound :: Pound -> String

-- given a float, the output should be a string,
-- first symbol of which is either the euro (€) or pound sign (£) repectively.
-- Can you manage to output the first two decimals of the float?
prettyprintPound p = "£" ++ (show p)
prettyprintEuro p = "€" ++ (show p)

---------------------------------------------------------------------
-- Exercise 7  escaping rules
---------------------------------------------------------------------

--rawtext :: String
--rawtext = ?

--text :: String
--text = putStr rawtext

-- uncomment the above two line, and try text in ghci window...

---------------------------------------------------------------------
-- Exercise 8  removeZeroes
---------------------------------------------------------------------

-- removeZeroes :: [Int] -> [Int]

---------------------------------------------------------------------
-- Exercise 9.  capslockon
---------------------------------------------------------------------

-- capslockson :: String -> String

--------------------------------------------------------------------
-- Exercise 10.  number of charachters in Char
---------------------------------------------------------------------

--listOfAllCharacters :: String

-- try and inspect with
--putStr listOfAllCharacters
-- how all these
-- characters look like
---------------------------------------------------------------------
-- Exercise 11.  removeZeroes2
---------------------------------------------------------------------

--example
--removeZeroes2 1020304 = 1234
--removeZeroes2 0 = "input should not be 0"

-- removeZeroes2 :: [Int] -> [Int]

