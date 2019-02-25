--------------------------------------------------------------------
-- CO2008  Functional Programming                            
-- Created: Feb 2019, University of Leicester, UK                        
--------------------------------------------------------------------           
-- Student Name: Mohamed Abdulwahid Sharif-Farah
-- Student Number: 179029141
-- Student Login name: masf1
--------------------------------------------------------------------

--These question can and should be answered using all the functions as mentioned on the
--slides. Don't borrow library functions from the web. If in doubt ask the lecturers or TAs.
--After all the point is to teach you how to write this kind of code...

module Worksheet3 where 
import Data.Char


----------------------------------------------------------------------
-- Exercise 1
---------------------------------------------------------------------
-- taking 0 of anything will not change result
-- taking something from an empty thing will give u the empty thing

skipall :: Int -> [a] -> [a]
skipall 0 a = a
skipall n [] = []
skipall n list = take (n-1) list ++ skipall n (drop n list)



----------------------------------------------------------------------
-- Exercise 2
---------------------------------------------------------------------

--First write homerge that merges two sorted lists into a sorted list

--homerge :: Ord b => (a -> b) -> [a] -> [a] -> [a]

   
--Now write the higher order merge sort

--hoMergeSort :: Ord b => (a -> b)  -> [a] -> [a]




----------------------------------------------------------------------
-- Exercise 3
---------------------------------------------------------------------


type Lastname = String 
type Username = String 
type Mark = Int

type Spreadsheet = [(Lastname, Username, Mark)]


--sortLastname :: Spreadsheet ->  Spreadsheet

--sortUsername :: Spreadsheet ->  Spreadsheet

--sortMark :: Spreadsheet ->  Spreadsheet



----------------------------------------------------------------------
-- Exercise 4
---------------------------------------------------------------------


--smallest :: Ord a => a -> [a] -> a

--delete :: Ord a => a -> [a] -> [a]

--bucketsort :: Ord a => [a] ->  [a]


----------------------------------------------------------------------
-- Exercise 5
---------------------------------------------------------------------

--Follow the instructions...

-- Let a tile be a list of usually equally long strings of characters

type Tile = [String]

-- Part a)


--makeTile :: Char -> Int -> [String]


-- Part b)

-- The tile ["****","****"] will be printed like:
-- ****
-- ****
-- using the following function (remove comments after writing tile2string)

--printTile :: Tile -> IO()
--printTile tile = putStr(tile2string (tile))


-- here tile2 string should convert the tile ["****","****"]
-- into the string "\n****\n****\n" (remember the newline character!)
-- tile2string ["****","****"] = "\n****\n****\n"

--tile2string :: [String] -> String


-- Part c)

-- write a function vglue that glues two tiles vericall like
--           &&&      ***             &&&
-- gluing of &&& and  *** should give &&&
--                                    ***
--                                    ***
 
--vglue :: Tile -> Tile -> Tile



-- Part d)

-- next write a function hglue that glues two tiles horizontally like
--           ***      ***             ******
-- gluing of *&* and  *** should give *&****
--           ***      ***             ******

--hglue :: Tile -> Tile -> Tile

-- Part e)

-- Next we want to print chessboards
-- so we introduce a type of boards in the form 
-- of a list of a list of tiles.

type Board = [[Tile]]

--Next function: delete comments after writing board2tile
--printBoard :: Board -> IO()
--printBoard board = printTile (board2tile board )

-- to print a board we first glue all its tiles together
-- using a function board2tile :: Board -> Tile
-- we need to help functions
--
-- col2tile  will glue a column of tiles vertically to a tile

--col2tile :: [Tile]->Tile



-- row2tile  will glue a row of tiles horizontally to a tile

--row2tile :: [Tile]->Tile


-- Part f)

-- So, if we think of a board as a column of rows of tiles,
-- then we can convert a board into a tile using  
-- col2tile and row2tile

--board2tile :: Board ->Tile


-- Part g)

-- if we can now make a function that "prints" an adge around a tile,
--  we can print boards with an edge.

--printBoardWithEdge :: Board -> IO()
--printBoardWithEdge board = printTile (edge (board2tile board ))



-- here the function edge :: Tile -> Tile should
-- produces a border around a rectangular tile:
-- .----.
-- |****|
-- |****|
-- .----.
-- edge (makeTile '*' 4) = [".----.","|****|","|****|",".----."]

--edge :: Tile -> Tile


-- Part h)

-- Finally: write a function that creates an nxn chessboard of 
-- "white" tiles of the form makeTile ' ' n
-- "black" tiles of the form makeTile '*' n
-- surrounded by a nice edge and
-- such that the square at the bottom left is black
-- EG chessboard 3 should give a tile that prints like
-- .----------------.
-- |    ****    ****|
-- |    ****    ****|
-- |****    ****    |
-- |****    ****    |
-- |    ****    ****|
-- |    ****    ****|
-- |****    ****    |
-- |****    ****    |
-- .----------------.

--chessboard :: Int -> Board


-- with the following function you can print such boards:

--chess :: Int -> IO()
--chess n = printBoardWithEdge (chessboard n)


