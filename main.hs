import TicTacToe

main :: IO()
main = do
    let grid = makeEmptyGrid 3
    gameLoop grid