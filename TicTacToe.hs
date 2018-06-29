module TicTacToe (
    makeEmptyGrid,
    gameLoop,
    gameAILoop
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
insertToGrid row col char grid = Map.insert col (Map.insert row char $ fromJust $ Map.lookup col grid) grid

flattenHelper :: [(Int, Map.Map Int a)] -> [(Int, a)] 
flattenHelper [x] = Map.toList(snd x)
flattenHelper (x:xs) = Map.toList (snd x) ++ flattenHelper xs

flatten :: Map.Map Int (Map.Map Int a) -> [a]
flatten map = snd $ unzip $ flattenHelper $ Map.toList map

diagonalPartialProg :: Int -> Int -> Int -> Bool -> Char -> String
diagonalPartialProg 0 size parts reverse c = []
diagonalPartialProg n size parts False c =
    (take (size-n) $ repeat '.') ++ [char] ++ (take (n-1) $ repeat '.') ++ 
        diagonalPartialProg (n-1) size (parts-1) False c
    where char = if parts > 0 then c; else '.'
diagonalPartialProg n size parts True c =
     (take (n-1) $ repeat '.') ++ [char] ++ (take (size-n) $ repeat '.') ++ 
        diagonalPartialProg (n-1) size (parts-1) True c
    where char = if parts > 0 then c; else '.'

diagonalPartial :: Bool -> Int -> Int -> Char -> String
diagonalPartial rev n parts c = diagonalPartialProg n n parts rev c

diagonal :: Bool -> Int -> Char -> String
diagonal rev size c = diagonalPartial rev size size c

diagonalMiddleProg :: Bool -> Int -> Int -> Char -> String
diagonalMiddleProg rev size 0 c = []
diagonalMiddleProg False size n c =
    (take (size-n) $ repeat '.') ++ [char] ++ (take (n-1) $ repeat '.') ++ 
        diagonalMiddleProg False size (n-1) c
    where char = if ceiling(fromIntegral size/2) == n then '.'; else c 
diagonalMiddleProg True size n c =
    (take (n-1) $ repeat '.') ++ [char] ++ (take (size-n) $ repeat '.') ++ 
        diagonalMiddleProg True size (n-1) c
    where char = if ceiling(fromIntegral size/2) == n then '.'; else c 

diagonalMiddle :: Bool -> Int -> Char -> Maybe String
diagonalMiddle rev size c
    | even size = Nothing
    | otherwise = Just(diagonalMiddleProg rev size size c)

verticalPartialProg :: Int -> Int -> Int -> Int -> Char -> String
verticalPartialProg n size 0 parts c = []
verticalPartialProg n size size1 parts c = (take (n-1) $ repeat '.') ++ [char] ++ 
        (take (size-n) $ repeat '.') ++ verticalPartialProg n size (size1-1) (parts-1) c
        where char = if parts > 0 then c; else '.'

verticalPartial :: Int -> Int -> Int -> Char -> String
verticalPartial n size parts c = verticalPartialProg n size size parts c

vertical :: Int -> Int -> Char -> String
vertical n size = verticalPartial n size size

verticalMiddleProg :: Int -> Int -> Int -> Char -> String
verticalMiddleProg n size 0 c = []
verticalMiddleProg n size size1 c = (take (n-1) $ repeat '.') ++ [char] ++
        (take (size-n) $ repeat '.') ++ verticalMiddleProg n size (size1-1) c
        where char = if ceiling(fromIntegral size/2) == size1 then '.'; else c 

verticalMiddle :: Int -> Int -> Char -> String
verticalMiddle n size c = verticalMiddleProg n size size c

dots :: Int -> Int -> String
dots rows 0 = []
dots rows cols = (take rows $ repeat '.') ++ dots rows (cols-1)

horizontalPartial :: Int -> Int -> Int -> Char -> String
horizontalPartial n size parts c = 
    dots size (size-n) ++ (take parts $ repeat c) ++ 
        (take (size - parts) $ repeat '.') ++ dots size (n-1)

horizontal :: Int -> Int -> Char -> String
horizontal n size = horizontalPartial n size size

horizontalMiddle :: Int -> Int -> Char -> String
horizontalMiddle n size c = 
    dots size (size-n) ++ (take (size `div` 2) $ repeat c) ++ 
        ['.'] ++ (take (size `div` 2) $ repeat c) ++ dots size (n-1)

patternMatch :: String -> String -> Bool
patternMatch [] [] = True
patternMatch (x:xs) (y:ys) = 
    (x == y || x == '.') && patternMatch xs ys 

compareDiagonal :: String -> Char -> Int -> Bool
compareDiagonal flatgrid c size = 
    patternMatch (diagonal True size c) flatgrid || patternMatch (diagonal False size c) flatgrid 

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

specialRem :: Int -> Int -> Int 
specialRem index size
    | r == 0 = size
    | otherwise = r
    where r = index `rem` size 

getGridPos :: Int -> Int -> (Int, Int)
getGridPos size index = 
    (index `specialRem` size, ceiling((fromIntegral index)/(fromIntegral size)))

getGridPosOfChar :: String -> Int -> Char -> [(Int, Int)]
getGridPosOfChar flatgrid size c = map (getGridPos size) $ map (\x -> x + 1) (elemIndices c flatgrid)

differenceStrs :: String -> String -> Char -> String
differenceStrs str1 str2 c = 
    zipWith (\x y -> if x /= y && x /= '.' then c else '.') str1 str2

data Direction = Vertical | Horizontal | Diagonal
    deriving(Enum, Eq, Show)

data Partial = Partial {
    direction :: Direction,
    parts :: Int,
    reversed :: Bool,
    dreversed :: Bool,
    n :: Maybe Int
}   deriving (Eq, Show)

findaDiagonalPartialProg :: String -> Int -> Int -> Char -> [Partial]
findaDiagonalPartialProg str 0 size c  
    | even size = []
    | patternMatch (fromJust $ diagonalMiddle False size c) str = 
        Partial{
            direction = Diagonal,
            parts = 0,
            dreversed = False,
            reversed = False,
            n = Nothing
            }:[]
    | patternMatch (fromJust $ diagonalMiddle True size c) str = 
        Partial{
            direction = Diagonal,
            parts = 0,
            dreversed = True,
            reversed = False,
            n = Nothing
            }:[]
    | otherwise = []
findaDiagonalPartialProg str parts size c
    | patternMatch (diagonalPartial False size parts c) str = 
         Partial{
             direction = Diagonal, 
             parts = parts, 
             dreversed = False, 
             reversed = False, 
             n = Nothing
             }:findaDiagonalPartialProg str (parts-1) size c
    | patternMatch (diagonalPartial True size parts c) str = 
        Partial{
            direction = Diagonal, 
            parts = parts, 
            dreversed = True, 
            reversed = False, 
            n = Nothing
            }:findaDiagonalPartialProg str (parts-1) size c
    | otherwise = findaDiagonalPartialProg str (parts-1) size c

findMiddles :: [Partial] -> [Partial]
findMiddles = filter ((== 0) . parts)

findaDiagonalPartial :: String -> Int -> Char -> Maybe Partial
findaDiagonalPartial str size char
    | items == [] = Nothing
    | (findMiddles items) /= [] = Just $ head $ findMiddles items
    | otherwise = Just $head items 
    where items = findaDiagonalPartialProg str size size char

findDiagonalPartials :: String -> Int -> Char -> [Partial]
findDiagonalPartials str size char
    | reversePartial /= [] = 
        (maybeToList $ findaDiagonalPartial str size char) ++
            [Partial{
                direction = Diagonal, 
                parts = parts $ head reversePartial, 
                dreversed = dreversed $ head reversePartial,
                reversed = True, 
                n = Nothing
            }]
    | otherwise = (maybeToList $ findaDiagonalPartial str size char)
    where reversePartial = maybeToList $ findaDiagonalPartial (reverse str) size char

findaPartialProg :: (Int -> Int -> Int -> Char -> String) -> 
    (Int -> Int -> Char -> String) ->
    String -> Int -> Int -> Int -> Char -> Direction -> [Partial]
findaPartialProg partialf middlef str 0 n size c dir  
    | even size = []
    | patternMatch (middlef n size c) str =
        Partial{
            direction = dir,
            parts = 0,
            reversed = False,
            dreversed = False,
            n = Just n
        }:[]
    | patternMatch (middlef n size c) $ reverse str =
        Partial{
            direction = dir,
            parts = 0,
            reversed = True,
            dreversed = False,
            n = Just n
        }:[]
    | otherwise = []
findaPartialProg partialf middlef str parts n size c dir
    | patternMatch (partialf n size parts c) str = 
        Partial{
        direction = dir, 
        parts = parts, 
        reversed = False, 
        dreversed = False,
        n = Just n
        }:findaPartialProg partialf middlef str (parts - 1) n size c dir
    | patternMatch (partialf n size parts c) $ reverse str =
        Partial{
            direction = dir,
            parts = parts,
            reversed = True,
            dreversed = False,
            n = Just n
        }:findaPartialProg partialf middlef str (parts - 1) n size c dir
    | otherwise = findaPartialProg partialf middlef str (parts - 1) n size c dir 

findaPartial :: (Int -> Int -> Int -> Char -> String) ->
    (Int -> Int -> Char -> String) ->
    String -> Int -> Int -> Char -> Direction -> Maybe Partial
findaPartial partialf middlef str n size c dir
    | items == [] = Nothing
    | (findMiddles items) /= [] = Just $head $ findMiddles items
    | otherwise = Just $head items
    where items = findaPartialProg partialf middlef str size n size c dir

findPartialsProg :: (Int -> Int -> Int -> Char -> String) -> 
    (Int -> Int -> Char -> String) ->
    String -> Int -> Int -> Char -> Direction -> [Partial]
findPartialsProg partialf middlef str size 0 c dir = []
findPartialsProg partialf middlef str size n c dir = 
    (maybeToList $ findaPartial partialf middlef str n size c dir) ++ 
        findPartialsProg partialf middlef str size (n-1) c dir

findPartials :: (Int -> Int -> Int -> Char -> String) ->
    (Int -> Int -> Char -> String) ->
    String -> Int -> Char -> Direction -> [Partial]
findPartials partialf middlef str size c dir = 
    findPartialsProg partialf middlef str size size c dir 
    
findVerticalPartials :: String -> Int -> Char -> [Partial]
findVerticalPartials str size c = findPartials verticalPartial verticalMiddle str size c Vertical

findHorizontalPartials :: String -> Int -> Char -> [Partial]
findHorizontalPartials str size c = findPartials horizontalPartial horizontalMiddle str size c Horizontal

findAllPartials :: String -> Int -> Char -> [Partial]
findAllPartials str size c = 
    findVerticalPartials str size c ++ findHorizontalPartials str size c ++
        findDiagonalPartials str size c 

getPartialString :: Partial -> Int -> Char -> String 
getPartialString Partial{direction = Horizontal, parts = 0, reversed = False, dreversed = d, n = num} size c =
    horizontalMiddle (fromJust num) size c
getPartialString Partial{direction = Vertical, parts = 0, reversed = False, dreversed = d, n = num} size c = 
    verticalMiddle (fromJust num) size c
getPartialString Partial{direction = Diagonal, parts = 0, reversed = False, dreversed = d, n = num} size c =
    fromJust (diagonalMiddle d size c) 
getPartialString Partial{direction = Horizontal, parts = p, reversed = False, dreversed = d, n = num} size c = 
    horizontalPartial (fromJust num) size p c
getPartialString Partial{direction = Vertical, parts = p, reversed = False, dreversed = d, n = num} size c = 
    verticalPartial (fromJust num) size p c
getPartialString Partial{direction = Diagonal, parts = p, reversed = False, dreversed = d, n = num} size c = 
    diagonalPartial d size p c
getPartialString Partial{direction = dir, parts = p, reversed = True, dreversed = d, n = num} size c =
    reverse $ getPartialString Partial{direction = dir, parts = p, reversed = False, dreversed = d, n = num} size c

getNextPartialString :: Partial -> Int -> Char -> String
getNextPartialString Partial{direction = dir, parts = 0, reversed = r, dreversed = d, n = num} size c 
    = getPartialString Partial {direction = dir, parts = size, reversed = r, dreversed = d, n = num} size c
getNextPartialString Partial{direction = dir, parts = p, reversed = r, dreversed = d, n = num} size c
    = getPartialString Partial{direction = dir, parts = (p+1), reversed = r, dreversed = d, n = num} size c

getNextPosition :: Int -> Char -> Partial -> (Int, Int)
getNextPosition size c partial = 
    last $ getGridPosOfChar 
        (differenceStrs (getNextPartialString partial size c) (getPartialString partial size c) c)
        size c

get2ndPartials :: Int -> [Partial] -> [Partial]
get2ndPartials size = filter (\x -> parts x == (size-1) || parts x == 0) 

block :: Map.Map Int (Map.Map Int Char) -> Char -> (Int, Int)
block grid char
    | length(filter (==char) flatgrid) > (size-2) = 
        last $ map (getNextPosition size char) 
            $ get2ndPartials size $ findAllPartials flatgrid size char
    | otherwise = head $ map (getNextPosition size char) $ findAllPartials flatgrid size char
    where flatgrid = replaceExcept (flatten grid) char
          size = gridSize grid

gameAILoop :: Map.Map Int (Map.Map Int Char) -> IO()
gameAILoop grid
    | isWinner grid 'x' && isWinner grid 'o' = do putStrLn "draw."
    | isWinner grid 'x' = do putStrLn "x is winner."
    | isWinner grid 'o' = do putStrLn "o is winner."
    | otherwise = do
        printGrid grid
        g <- turn grid 'x'
        let loc = block g 'x'
        let filledgrid = insertToGrid (fst loc) (snd loc) 'o' g
        gameAILoop filledgrid
    