module TicTacToe (
    makeEmptyGrid,
    gameLoop
)
where

import qualified Data.Map as Map
import Data.Maybe
import Data.List (elemIndices)

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

diagonalPartialProg :: Int -> Int -> Int -> Char -> String
diagonalPartialProg 0 size parts c = []
diagonalPartialProg n size parts c = 
    (take (size-n) $ repeat '.') ++ [char] ++ (take (n-1) $ repeat '.') ++ 
        diagonalPartialProg (n-1) size (parts-1) c
    where char = if parts > 0 then c; else '.'

diagonalPartial :: Int -> Int -> Char -> String
diagonalPartial n parts c = diagonalPartialProg n n parts c

diagonal :: Int -> Char -> String
diagonal n c = diagonalPartial n n c

verticalPartialProg :: Int -> Int -> Int -> Int -> Char -> String
verticalPartialProg n size 0 parts c = []
verticalPartialProg n size size1 parts c = (take (n-1) $ repeat '.') ++ [char] ++ 
        (take (size-n) $ repeat '.') ++ verticalPartialProg n size (size1-1) (parts-1) c
        where char = if parts > 0 then c; else '.'

verticalPartial :: Int -> Int -> Int -> Char -> String
verticalPartial n size parts c = verticalPartialProg n size size parts c

vertical :: Int -> Int -> Char -> String
vertical n size = verticalPartial n size size

dots :: Int -> Int -> String
dots rows 0 = []
dots rows cols = (take rows $ repeat '.') ++ dots rows (cols-1)

horizontalPartial :: Int -> Int -> Int -> Char -> String
horizontalPartial n size parts c = 
    dots size (size-n) ++ (take parts $ repeat c) ++ 
        (take (size - parts) $ repeat '.') ++ dots size (n-1)

horizontal :: Int -> Int -> Char -> String
horizontal n size = horizontalPartial n size size

patternMatch :: String -> String -> Bool
patternMatch [] [] = True
patternMatch (x:xs) (y:ys) = 
    (x == y || x == '.') && patternMatch xs ys 

compareDiagonal :: String -> Char -> Int -> Bool
compareDiagonal flatgrid c size = 
    patternMatch diag flatgrid || patternMatch (reverse diag) flatgrid 
    where diag = diagonal size c

compareHorizontalProg :: String -> Char -> Int -> Int -> Bool
compareHorizontalProg flatgrid c size 0 = False
compareHorizontalProg flatgrid c size n = 
    patternMatch (horizontal n size c) flatgrid || compareHorizontalProg flatgrid c size (n-1)

compareHorizontal :: String -> Char -> Int -> Bool
compareHorizontal flatgrid c size = compareHorizontalProg flatgrid c size size

compareVerticalProg :: String -> Char -> Int -> Int -> Bool
compareVerticalProg flatgrid c size 0 = False
compareVerticalProg flatgrid c size n = 
    patternMatch (vertical n size c) flatgrid || compareVerticalProg flatgrid c size (n-1)

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

getGridPos :: Int -> Int -> (Int, Int)
getGridPos size index = 
    (index `rem` size, ceiling((fromIntegral index)/(fromIntegral size)))

getGridPosOfChar :: String -> Int -> Char -> [(Int, Int)]
getGridPosOfChar flatgrid size c = map (getGridPos size) $ elemIndices c flatgrid

differenceStrs :: String -> String -> Char -> String
differenceStrs str1 str2 c = 
    zipWith (\x y -> if x /= y || x /= '.' then c else '.') str1 str2

data Direction = Vertical | Horizontal | Diagonal
    deriving(Enum, Eq, Show)

data Partial = Partial {
    direction :: Direction,
    parts :: Int,
    reversed :: Bool,
    n :: Maybe Int
}   deriving (Eq, Show)

findaDiagonalPartialProg :: String -> Int -> Int -> Char -> [Partial]
findaDiagonalPartialProg str 0 size c = []
findaDiagonalPartialProg str parts size c
    | patternMatch (diagonalPartial size parts c) str = 
        Partial{direction = Diagonal, parts = parts, reversed = False, n = Nothing}:
            findaDiagonalPartialProg str (parts-1) size c
    | otherwise = findaDiagonalPartialProg str (parts-1) size c

findaDiagonalPartial :: String -> Int -> Char -> Maybe Partial
findaDiagonalPartial str size char
    | items == [] = Nothing
    | otherwise = Just $head items 
    where items = findaDiagonalPartialProg str size size char

findDiagonalPartials :: String -> Int -> Char -> [Partial]
findDiagonalPartials str size char
    | reversePartial /= [] = 
        (maybeToList $ findaDiagonalPartial str size char) ++
            [Partial{
                direction = Diagonal, 
                parts = parts $ head reversePartial, 
                reversed = True, 
                n = Nothing
            }]
    | otherwise = (maybeToList $ findaDiagonalPartial str size char)
    where reversePartial = maybeToList $ findaDiagonalPartial (reverse str) size char

findaPartialProg :: (Int -> Int -> Int -> Char -> String) -> 
    String -> Int -> Int -> Int -> Char -> Direction -> [Partial]
findaPartialProg partialf str 0 n size c dir = []
findaPartialProg partialf str parts n size c dir
    | patternMatch (partialf n size parts c) str = 
        Partial{direction = dir, parts = parts, reversed = False, n = Just n}:
            findaPartialProg partialf str (parts - 1) n size c dir 
    | otherwise = findaPartialProg partialf str (parts - 1) n size c dir 

findaPartial :: (Int -> Int -> Int -> Char -> String) ->
    String -> Int -> Int -> Char -> Direction -> Maybe Partial
findaPartial partialf str n size c dir
    | items == [] = Nothing
    | otherwise = Just $head items
    where items = findaPartialProg partialf str size n size c dir

findPartialsProg :: (Int -> Int -> Int -> Char -> String) -> 
    String -> Int -> Int -> Char -> Direction -> [Partial]
findPartialsProg partialf str size 0 c dir = []
findPartialsProg partialf str size n c dir = 
    (maybeToList $ findaPartial partialf str n size c dir) ++ 
        findPartialsProg partialf str size (n-1) c dir

findPartials :: (Int -> Int -> Int -> Char -> String) ->
    String -> Int -> Char -> Direction -> [Partial]
findPartials partialf str size c dir = 
    findPartialsProg partialf str size size c dir

findVerticalPartials :: String -> Int -> Char -> [Partial]
findVerticalPartials str size c = findPartials verticalPartial str size c Vertical

findHorizontalPartials :: String -> Int -> Char -> [Partial]
findHorizontalPartials str size c = findPartials horizontalPartial str size c Horizontal

findAllPartials :: String -> Int -> Char -> [Partial]
findAllPartials str size c = 
    findVerticalPartials str size c ++ findHorizontalPartials str size c ++
        findDiagonalPartials str size c 

getPartialString :: Partial -> Int -> Char -> String 
getPartialString Partial{direction = Horizontal, parts = p, reversed = r, n = num} size c = 
    horizontalPartial (fromJust num) size p c
getPartialString Partial{direction = Vertical, parts = p, reversed = r, n = num} size c = 
    verticalPartial (fromJust num) size p c
getPartialString Partial{direction = Diagonal, parts = p, reversed = r, n = num} size c = 
    diagonalPartial size p c

getNextPartialString :: Partial -> Int -> Char -> String
getNextPartialString Partial{direction = dir, parts = p, reversed = r, n = num} 
    = getPartialString Partial{direction = dir, parts = (p+1), reversed = r, n = num}

getNextPosition :: Int -> Char -> Partial -> (Int, Int)
getNextPosition size c partial = 
    head $ getGridPosOfChar 
        (differenceStrs (getNextPartialString partial size c) (getPartialString partial size c) c)
        size c

get2ndPartials :: Int -> [Partial] -> [Partial]
get2ndPartials size = filter (\x -> parts x == (size-1)) 

block :: Map.Map Int (Map.Map Int Char) -> Char -> (Int, Int)
block grid char
    | length(filter (==char) flatgrid) > (size-2) = 
        head $ map (getNextPosition size char) 
            $ get2ndPartials size $ findAllPartials flatgrid size char
    | otherwise = head $ map (getNextPosition size char) $ findAllPartials flatgrid size char
    where flatgrid = replaceExcept (flatten grid) char
          size = gridSize grid
