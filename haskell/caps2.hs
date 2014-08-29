import Data.Char -- toUpper

main = contents <- getContents
    putStr $ map toUpper contents
