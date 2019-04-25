-- Testing basic command line Game of Life
import Data.Char
import Control.Monad
import System.Exit
import Text.Regex.Posix


-- this is to counter the windows ghci bug in getChar
-- comment this out and change getHiddenChar to getChar to make it work on Mac machines
{-import Foreign.C.Types
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt-}


{-

Credit the "tick" page we used
Credit https://github.com/igstan/programming-in-haskell/blob/master/game-of-life.hs
-}

type Cell = (Int, Int)
type Grid = [[Int]]
type AliveCells = [(Int, Int)]
type ListRules = [Int]

-- Creates a blank grid of size x y
blank_grid :: Int -> Int -> Grid
blank_grid x y 
    | y == 0 = []
    | otherwise = replicate x 0 : blank_grid x (y-1)


-- Takes a grid and a list of tuples of alive states and creates the corresponding grid
grid_creator :: Grid -> AliveCells -> Grid
grid_creator g a = foldl format_cell g a

-- Takes a grid and a tuple of the cell to be made "alive" and returns a grid with that cell "alive"
format_cell :: Grid -> Cell -> Grid 
format_cell g (x, y) = d ++ (d1 ++ 1 : f1) : t
    where
        (d,f) = splitAt y g
        (h:t) = f
        (d1, _ : f1) = splitAt x h
        

-- neighbors takes a cell and returns a list of cells which are the neighbors of the given cell
neighbors :: Cell -> [Cell]
neighbors (x, y) = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

-- Takes a cell and a grid, and returns the number of neighbors that cell has that are alive
number_neighbors_alive :: Cell -> Grid -> Int
number_neighbors_alive c g = foldr (+) 0 (map (get_value_cell g) n)
    where
        n = neighbors c

-- Takes a grid and a cell and returns the value of that cell
get_value_cell :: Grid -> Cell -> Int
get_value_cell g (x, y)
    | x > lx = 0
    | y > ly = 0
    | x < 0 = 0
    | y < 0 = 0
    | otherwise = r 
        where
            lx = length (head g) - 1
            ly = length g - 1
            (d,f) = splitAt y g 
            (h:t) = f 
            (h1,t2) = splitAt x h
            r = head t2 


-- Rules
-- dead_or_alive takes a grid and a cell, and determines if the cell should be alive in the next state
dead_or_alive :: Grid -> Cell -> ListRules -> Int
dead_or_alive g c lv 
    | v == 1 && n < head lv = 0
    | v == 1 && n >= head lv && n <= head (tail lv) = 1
    | v == 1 && n > head (tail lv) = 0
    | v == 0 && n >= head (t1) && n <= head (t2) = 1
    | otherwise = v
    where 
        v = get_value_cell g c
        n = number_neighbors_alive c g
        (h1, t1) = splitAt 2 lv
        (h2, t2) = splitAt 3 lv



-- next_state takes a grid and creates the next state for it based on the rules
-- Applies dead_or_alive to each cell then creates a grid with 0 for dead cells and 1 for alive cells
next_state :: Grid -> ListRules -> Grid
next_state g lv = next_state_helper g x y x y lv (blank_grid (x+1) (y+1))
    where
        x = length (head g) - 1 
        y = length g - 1


next_state_helper :: Grid -> Int -> Int -> Int -> Int -> ListRules -> Grid -> Grid 
next_state_helper g x y ox oy lv acc
    | x == 0 && y == 0 = change_cell g (x,y) lv acc 
    | x == 0 = next_state_helper g ox (y-1) ox oy lv (change_cell g (x,y) lv acc)
    | otherwise = next_state_helper g (x-1) y ox oy lv (change_cell g (x,y) lv acc)



-- Takes a grid and a cell to change and returns the acc grid with 1 if the cell is alive
change_cell :: Grid -> Cell -> ListRules -> Grid -> Grid
change_cell g c lv acc 
    | dead_or_alive g c lv == 0 = acc
    | otherwise = format_cell acc c

-- Checks that the user input is a digit
valid_input :: Char -> IO()
valid_input a = 
    if (isDigit a)
        then
            putStrLn ""
        else
            do
                putStrLn "\nNOT A VALID INPUT. Please restart"
                main 
                exitSuccess

-- Checks that the range fot the number of neighbours is in the form n_start < n_end
valid_range :: Char -> Char -> IO()
valid_range a b =
    if ((digitToInt a) > (digitToInt b))
        then
            do
                putStrLn "\nThis digit needs to be greater than the start range. Please restart"
                main
                exitSuccess
        else
             putStrLn "" 

-- Checks that the axis sizes are given as digits; using "return" is counted as an invalid input
valid_input_axis :: [Char] -> IO()
valid_input_axis c = 
    if (all isDigit c) && (c /= "")
        then
            putStrLn ""
        else
            do
                putStrLn "\nPlease only input Digits."
                main
                exitSuccess


-- Checks that the initial alive cells are inside the grid boundaries
valid_input_starting_state :: AliveCells -> Int -> Int -> IO()
valid_input_starting_state g x y = 
    if (all (\ (a,b) -> (a<x && b<y)) g)
        then 
              putStrLn ""
        else
            do
                putStrLn "\nOnly input alive cells which are within the starting boundries"
                main
                exitSuccess

-- Checks that user has entered custom alive cells in correct format
valid_cells :: [Char] -> IO()
valid_cells(userStart)=
     if (userStart =~ "[[][(][0-9]+,[0-9]+[)](,[(][0-9]+,[0-9]+[)])*[]]")
        then 
              putStrLn ""
        else
            do
                putStrLn "\nPlease input alive cells in correct format of [(x1,y1),(x2,y2)...]"
                main
                exitSuccess

        
none_alive :: Grid -> Bool
none_alive g
    | g == [] = True
    | contains_one $ head g = False
    | otherwise = none_alive $ tail g

contains_one :: [Int] -> Bool
contains_one lst 
    | lst == [] = False
    | head lst == 1 = True
    | otherwise = contains_one $ tail lst

-- Focuses the screen on the new state
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Asks for an input to create a grid with a chosen starting pattern
start_grid_creator :: ListRules -> IO()
start_grid_creator lv = do
    putStrLn "\nWhat size x axis would you like?"
    x <- getLine
    valid_input_axis x
    putStrLn "\nWhat size y axis would you like?"
    y <- getLine
    valid_input_axis y
    let blankGrid = blank_grid (read x :: Int) (read y :: Int)
    putStrLn "\nWhich cells would you like to start as alive? \nSelect a number from 1 to 9 for preset starts \nor type m to enter your own start \n1. Blinker Oscillator - best grid size 5x5\n2. Block Still Life - min grid size 4x4\n3. Quick Death - best grid size 5x5\n4. Still Death\n5. Glider becomes Still Life - min grid size 7x7\n6. Pulsar - min grid size 16x16\n7. Boat - min grid size 5x5\n8. Snake - min grid size 5x5 \n9. Long Boat - min grid size 5x5"
    z <- getChar
    if ((not $ isDigit z) && z /= 'm')
        then
            do
                putStrLn "\nPlease only enter digits or m"
                main
                exitSuccess
        else 
            putStrLn ""
    if (z == '1') 
        then 
            do 
                let startGrid = grid_creator blankGrid [(1, 2), (2, 2), (3, 2)] 
                valid_input_starting_state [(1, 2), (2, 2), (3, 2)](read x :: Int) (read y :: Int)
                mapM_ print startGrid
                putStrLn "\n"
                intput_key_useless <- getLine
                waitForNextTick (startGrid) lv
        else if (z == '2')
            then
            do
                let startGrid = grid_creator blankGrid [(2, 1), (3, 1), (2, 2), (3, 2)] 
                valid_input_starting_state [(2, 1), (3, 1), (2, 2), (3, 2)] (read x :: Int) (read y :: Int)
                mapM_ print startGrid
                putStrLn "\n"
                intput_key_useless <- getLine
                waitForNextTick (startGrid) lv
        else if (z == '3')
            then
            do
                let startGrid = grid_creator blankGrid [(1, 2), (2, 3), (3, 3), (4, 3)] 
                valid_input_starting_state [(1, 2), (2, 3), (3, 3), (4, 3)] (read x :: Int) (read y :: Int)
                mapM_ print startGrid 
                putStrLn "\n"
                intput_key_useless <- getLine
                waitForNextTick (startGrid) lv
        else if (z == '4')
            then
            do
                let startGrid = grid_creator blankGrid [] 
                mapM_ print startGrid
                putStrLn "\n"
                intput_key_useless <- getLine
                waitForNextTick (startGrid) lv
        else if (z == '5')
            then
            do
                let startGrid = grid_creator blankGrid [(2, 1), (5, 1), (6, 2), (2, 3), (6, 3), (3, 4), (4, 4), (5, 4), (6, 4)] 
                valid_input_starting_state [(2, 1), (5, 1), (6, 2), (2, 3), (6, 3), (3, 4), (4, 4), (5, 4), (6, 4)]  (read x :: Int) (read y :: Int)
                mapM_ print startGrid
                putStrLn "\n"
                intput_key_useless <- getLine
                waitForNextTick (startGrid) lv
        else if (z == '6')
            then
            do
                let startGrid = grid_creator blankGrid [(4,2), (5,2), (6,2), (10,2), (11,2), (12,2),
                                                        (2,4), (7,4), (9,4), (14,4),
                                                        (2,5), (7,5), (9,5), (14,5),
                                                        (2,6), (7,6), (9,6), (14,6),
                                                        (4,7), (5,7), (6,7), (10,7), (11,7), (12,7),
                                                        (4,9), (5,9), (6,9), (10,9), (11,9), (12,9),
                                                        (2,10), (7,10), (9,10), (14,10),
                                                        (2,11), (7,11), (9,11), (14,11),
                                                        (2,12), (7,12), (9,12), (14,12),
                                                        (4,14), (5,14), (6,14), (10,14), (11,14), (12,14)] 
                valid_input_starting_state [(4,2), (5,2), (6,2), (10,2), (11,2), (12,2),
                                                        (2,4), (7,4), (9,4), (14,4),
                                                        (2,5), (7,5), (9,5), (14,5),
                                                        (2,6), (7,6), (9,6), (14,6),
                                                        (4,7), (5,7), (6,7), (10,7), (11,7), (12,7),
                                                        (4,9), (5,9), (6,9), (10,9), (11,9), (12,9),
                                                        (2,10), (7,10), (9,10), (14,10),
                                                        (2,11), (7,11), (9,11), (14,11),
                                                        (2,12), (7,12), (9,12), (14,12),
                                                        (4,14), (5,14), (6,14), (10,14), (11,14), (12,14)] (read x :: Int) (read y :: Int)
                mapM_ print startGrid
                putStrLn "\n"
                intput_key_useless <- getLine
                waitForNextTick (startGrid) lv
        else if (z == '7')
            then
            do
                let startGrid = grid_creator blankGrid [(3,2), (2,3), (4,3), (3,4), (4,4)] 
                
                valid_input_starting_state [(3,2), (2,3), (4,3), (3,4), (4,4)] (read x :: Int) (read y :: Int)

                mapM_ print startGrid
                putStrLn "\n"
                intput_key_useless <- getLine
                waitForNextTick (startGrid) lv
        else if (z == '8')
            then
            do
                let startGrid = grid_creator blankGrid [(1,3), (3,3), (4,3), (1,4), (2,4), (4,4)] 
                
                valid_input_starting_state [(1,3), (3,3), (4,3), (1,4), (2,4), (4,4)] (read x :: Int) (read y :: Int)

                mapM_ print startGrid
                putStrLn "\n"
                intput_key_useless <- getLine
                waitForNextTick (startGrid) lv
        else if (z == '9')
            then
            do
                let startGrid = grid_creator blankGrid [(3,1), (2,2), (4,2), (3,3), (5,3), (4,4), (5,4)] 
                
                valid_input_starting_state [(3,1), (2,2), (4,2), (3,3), (5,3), (4,4), (5,4)] (read x :: Int) (read y :: Int)

                mapM_ print startGrid
                putStrLn "\n"
                intput_key_useless <- getLine
                waitForNextTick (startGrid) lv


-- this is a working part that i want to replace with part below       

        else
            do
                putStrLn "\nPlease input enter your own start in the form of [(x, y), (x2, y2) ..]" 
                userStart <- getLine
                if userStart == ""
                    then
                        do
                            let startGrid = grid_creator blankGrid []
                            mapM_ print startGrid
                            putStrLn "\n"
                            intput_key_useless <- getLine
                            waitForNextTick (startGrid) lv
                    else
                        do
                            valid_cells(userStart)
                            let startGrid = grid_creator blankGrid (read userStart :: [Cell])
                            valid_input_starting_state (read userStart :: [Cell]) (read x :: Int) (read y :: Int)
                            mapM_ print startGrid
                            putStrLn "\n"
                            intput_key_useless <- getLine
                            waitForNextTick (startGrid) lv
     


-- The main method that changes the state in the game and prompts the user to provide input values
main = do 
    putStrLn "\nIf you would like to use the original rules, type 1, otherwise type 2"
    w <- getChar
    if(w == '2')
         then
            do  
                putStrLn (show w)
                putStrLn "What is the start of the range of number of neighbors alive needed for an alive cell to remain alive?"
                nns <- getChar
                putStrLn (show nns)
                valid_input nns
                putStrLn "\nWhat is the end of the range of number of neighbors alive needed for an alive cell to remain alive?"
                nne <- getChar
                putStrLn (show nne)
                valid_input nne
                valid_range nns nne 
                putStrLn "\nWhat is the start of the range of number of neighbors alive needed for a dead cell to become alive?"
                nnas <- getChar
                valid_input nnas
                putStrLn (show nnas)
                putStrLn "\nWhat is the end of the range of number of neighbors alive needed for a dead cell to become alive?"
                nnae <- getChar
                putStrLn (show nnae)
                valid_input nnae
                valid_range nnas nnae 
                start_grid_creator [(digitToInt nns), (digitToInt nne), (digitToInt nnas), (digitToInt nnae)]
        else if (w == '1')
            then
            do
                putStrLn (show w)
                start_grid_creator [2, 3, 3, 3]
        else 
            do 
                putStrLn "\nPlease only input 1 or 2"
                main
                exitSuccess


-- Change the state of the game with each tick
waitForNextTick g lv = do
    let nextState = next_state g lv
    clearScreen
    mapM_ print nextState
    if (none_alive nextState)
        then
            do 
                putStrLn "\nThere are no alive states remaining.\nThanks for using our Haskell interpretation of Conway's Game of Life!"
                exitSuccess
        else
            putStrLn ""
    inputKey <- getLine
    when (not (inputKey == "q")) $ do
        --waitForNextTick nextState lv
    putStrLn "\nThanks for using our Haskell interpretation of Conway's Game of Life!"
    exitSuccess



{-
Example Start States
On a 5x5 grid
[(1, 2), (2, 2), (3, 2)]

Blank Grid of 5x5
[[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]

Testing
format_cell [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]] (1, 2)
g = grid_creator [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]] [(1, 2), (2, 2), (3, 2)]

neighbors (1,2)

number_neighbors_alive (1, 2) [[0,0,0,0,0],[0,0,0,0,0],[0,1,1,1,0],[0,0,0,0,0],[0,0,0,0,0]]
number_neighbors_alive (2, 2) [[0,0,0,0,0],[0,0,0,0,0],[0,1,1,1,0],[0,0,0,0,0],[0,0,0,0,0]]


dead_or_alive [[0,0,0,0,0],[0,0,0,0,0],[0,1,1,1,0],[0,0,0,0,0],[0,0,0,0,0]] (1,2)
dead_or_alive [[0,0,0,0,0],[0,0,0,0,0],[0,1,1,1,0],[0,0,0,0,0],[0,0,0,0,0]] (2,2)
dead_or_alive [[0,0,0,0,0],[0,0,0,0,0],[0,1,1,1,0],[0,0,0,0,0],[0,0,0,0,0]] (3,2) 

dead_or_alive [[0,0,0,0,0],[0,0,0,0,0],[0,1,1,1,0],[0,0,0,0,0],[0,0,0,0,0]] (2,1)
dead_or_alive [[0,0,0,0,0],[0,0,0,0,0],[0,1,1,1,0],[0,0,0,0,0],[0,0,0,0,0]] (2,3)

[(0,1), (0,2), (1,2)]
[(0,1), (0,2)]


test states for 
glider/spaceship/gun
glider [(2,4),(3,2),(4,3),(4,4),(3,4)] 

2 (own rules) 1 3 1 3 10 10 1 (blinker oscillator)
-}