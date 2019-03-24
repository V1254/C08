
-- CO 2008 Functional Programming 
-- Created: March 2019, University of Leicester, UK 
-------------------------------------------------------------------- 
-- Student Name: Mohamed Abdulwahid Sharif-Farah 
-- Student Number: 179029141 
--------------------------------------------------------------------
--
-- Please don't use the internet or your friends; 
-- instead consult the slides.
-- On the slides you find explanantion regarding 
-- Trees and Error type.
-- this should be your own work
--

module Worksheet4 where 

---------------------------------------------------------------------
----- EXERCISE 1
---------------------------------------------------------------------
data Value = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|J|Q|K|A
             deriving (Eq, Ord, Enum)
instance Show Value where
--- Part a)
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show J = "J"
    show Q  = "Q"
    show K = "K"
    show A = "A"

data Suite = Hearts | Spades | Diamonds | Clubs
             deriving (Eq, Ord, Enum)
instance Show Suite where
--- Part b)
    show Hearts = "H"
    show Spades = "S"
    show Diamonds = "D"
    show Clubs = "C"

data Colour = Red | Black
              deriving (Eq, Ord,Enum, Show)

data Error a = Fail|Ok a
               deriving (Eq, Ord, Show)



type Card  = (Value, Suite)

--- Part c)
--  function to transform a card into its string component
showCard :: Card -> String
showCard (a,b) = (show a) ++ (show b)

pack :: [Card]
pack = [(val, suit) | val <- [(Two)..(A)], suit <- [(Hearts)..(Clubs)]]

--- Part d)

colour :: Card -> Colour
colour (val,suit)
    | suit == Diamonds || suit == Hearts  = Red
    | otherwise = Black


--- Part e)

split :: Int -> [a] -> (Error ([a],[a]))
split n l
    | n > (length l) || n < 0 = Fail
    | otherwise = Ok(take n l, drop n l)


interleave ::  [a] ->  [a] -> [a]
interleave [] l = l
interleave l [] = l
interleave (x:xs) (y:ys) = x:y: (interleave xs ys)


--- Part f)

shuffle :: [Int] -> [a] -> Error [a]
shuffle [] l = Ok l
shuffle (x:xs) l = case split x l of
    Fail -> Fail
    Ok (a, b) -> shuffle xs (interleave a b)

---------------------------------------------------------------------
----- EXERCISE 2
---------------------------------------------------------------------


data Btree a = ND | Data a |  Branch (Btree a) (Btree a)
               deriving (Show,Eq)

data Dir = L | R 
           deriving (Show,Eq)

type Path =  [Dir] 
    
--- Part a)

extract :: Path  -> Btree a -> Error a
extract [] (Data b) = Ok b
extract [] t = Fail
extract (x:xs) (Branch left right)
    | x == L = extract xs left
    | x == R = extract xs right
extract (x:xs) t = Fail



--- Part b)

add :: a -> Path -> Btree a -> Error (Btree a)
add a [] (ND) = Ok (Data a)
add a (L:xs) (Branch left right) = case add a xs left of
                                   Fail -> Fail
                                   Ok lefft -> Ok (Branch lefft right)
add a (R:xs) (Branch left right) = case add a xs right of
                                   Fail -> Fail
                                   Ok righht -> Ok (Branch left righht)

add a b c = Fail -- something wrong if we get here.



--- Part c)

findpath :: Eq b => (a -> b) -> b -> Btree a -> [Path]

-- test for no data.
findpath a b ND = []
findpath f a (Data b)
    | f b == a = [[]]
    | otherwise = []

findpath f a (Branch left right) = (map ((L):)(findpath f a left)) ++ (map ((R):)(findpath f a right))


tree1 = Branch ND ND
tree2 = Branch ND (Data 3)
tree3 = Branch tree1 tree2
tree4 = Branch (Data 3) (Data 4)
tree5 = Branch tree3 tree4


--Don't forget your name

-- please take care that your solution compiles.
-- of course if things don't work, you can comment them out
-- and explain in the comment that that something is wrong with it.
