module TicTacToe ()
where

import qualified Data.Map as Map
import Data.Maybe

makeEmptyListGrid :: Int -> Int -> [Map.Map Int Char]
makeEmptyListGrid 0 rows = []
makeEmptyListGrid cols rows = 
    [Map.fromList $ zip [1..] $ take rows $ repeat ' '] ++ makeEmptyListGrid (cols-1) rows

makeEmptyGrid :: Int -> Int -> Map.Map Int (Map.Map Int Char)
makeEmptyGrid cols rows = Map.fromList $ zip [1..] $ makeEmptyListGrid cols rows

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
    (take (size-n) $ repeat ' ') ++ [c] ++ (take (n-1) $ repeat ' ') ++ 
        diagonalProg (n-1) size c

diagonal :: Int -> Char -> String
diagonal n c = diagonalProg n n c