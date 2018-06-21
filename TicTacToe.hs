module TicTacToe (
    makeEmptyGrid,
    gameLoop
)
where

import qualified Data.Map as Map
import Data.Maybe

makeEmptyListGrid :: Int -> Int -> [Map.Map Int Char]
makeEmptyListGrid 0 rows = []
makeEmptyListGrid cols rows = 
    [Map.fromList $ zip [1..] $ take rows $ repeat '.'] ++ makeEmptyListGrid (cols-1) rows

makeEmptyGrid :: Int -> Map.Map Int (Map.Map Int Char)
makeEmptyGrid size = Map.fromList $ zip [1..] $ makeEmptyListGrid size size

insertToGrid :: Int -> Int -> Char -> Map.Map Int (Map.Map Int Char) -> Map.Map Int (Map.Map Int Char)
insertToGrid col row char grid = Map.insert col (Map.insert row char $ fromJust $ Map.lookup col grid) grid

flattenHelper :: [(Int, Map.Map Int a)] -> [(Int, a)] 
flattenHelper [x] = Map.toList(snd x)
flattenHelper (x:xs) = Map.toList (snd x) ++ flattenHelper xs

flatten :: Map.Map Int (Map.Map Int a) -> [a]
flatten map = snd $ unzip $ flattenHelper $ Map.toList map

diagonalProg :: Int -> Int -> Char -> String
diagonalProg 0 size c = []
diagonalProg n size c = 
    (take (size-n) $ repeat '.') ++ [c] ++ (take (n-1) $ repeat '.') ++ 
        diagonalProg (n-1) size c

diagonal :: Int -> Char -> String
diagonal n c = diagonalProg n n c

verticalProg :: Int -> Int -> Int -> Char -> String
verticalProg n size 0 c = []
verticalProg n size size1 c = (take (n-1) $ repeat '.') ++ [c] ++ 
    (take (size-n) $ repeat '.') ++ verticalProg n size (size1-1) c

vertical :: Int -> Int -> Char -> String
vertical n size c = verticalProg n size size c

dots :: Int -> Int -> String
dots rows 0 = []
dots rows cols = (take rows $ repeat '.') ++ dots rows (cols-1)

horizontal :: Int -> Int -> Char -> String
horizontal n size c = dots size (size-n) ++ (take size $ repeat c) ++ dots size (n-1)

-- assumes grid is square 
sizeOfGrid :: Map.Map Int (Map.Map Int Char) -> Int
sizeOfGrid grid = length $ Map.toList grid

compareDiagonal :: String -> Char -> Int -> Bool
compareDiagonal flatgrid c size
    | flatgrid == diag = True
    | flatgrid == reverse diag = True
    | otherwise = False
    where diag = diagonal size c

compareHorizontalProg :: String -> Char -> Int -> Int -> Bool
compareHorizontalProg flatgrid c size 0 = False
compareHorizontalProg flatgrid c size n = 
    (horizontal n size c == flatgrid) || compareHorizontalProg flatgrid c size (n-1)

compareHorizontal :: String -> Char -> Int -> Bool
compareHorizontal flatgrid c size = compareHorizontalProg flatgrid c size size

compareVerticalProg :: String -> Char -> Int -> Int -> Bool
compareVerticalProg flatgrid c size 0 = False
compareVerticalProg flatgrid c size n = 
    (vertical n size c == flatgrid) || compareVerticalProg flatgrid c size (n-1)

compareVertical :: String -> Char -> Int -> Bool
compareVertical flatgrid c size = compareVerticalProg flatgrid c size size

gridSize :: Map.Map Int (Map.Map Int Char) -> Int
gridSize grid = length $ Map.toList grid

replaceExcept :: String -> Char -> String
replaceExcept str char = map ((\char c -> if c /= char then '.'; else c) char) str

isWinner :: Map.Map Int (Map.Map Int Char) -> Char -> Bool
isWinner grid char = 
    let flatgrid = replaceExcept (flatten grid) char 
        size = gridSize grid
    in compareDiagonal flatgrid char size 
        || compareHorizontal flatgrid char size
        || compareVertical flatgrid char size

insert :: Int -> a -> [a] -> [a]
insert 0 y xs = xs
insert n y [] = []
insert n y xs
    | length xs < n = xs
    | otherwise = take n xs ++ [y] ++ insert n y (drop n xs)

printGrid :: Map.Map Int (Map.Map Int Char) -> IO()
printGrid grid = do
    let size = gridSize grid
    putStr $ insert size '\n' $ flatten grid

turn :: Map.Map Int (Map.Map Int Char) -> Char -> IO(Map.Map Int (Map.Map Int Char))
turn grid char = do
        putStrLn $ [char] ++ "'s turn:\nCol:"
        col <- readLn :: IO Int 
        putStrLn "Row:"
        row <- readLn :: IO Int
        return $ insertToGrid col row char grid 

gameLoop :: Map.Map Int (Map.Map Int Char) -> IO()
gameLoop grid 
    | isWinner grid 'x' && isWinner grid 'o' = do putStrLn "draw."
    | isWinner grid 'x' = do putStrLn "x is winner."
    | isWinner grid 'o' = do putStrLn "o is winner."
    | otherwise = do
        printGrid grid
        g <- turn grid 'x'
        filledgrid <- turn g 'o'
        gameLoop filledgrid
        