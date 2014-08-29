import Data.Char -- toUpper

main = do
    contents <- getContents
    putStr $ map toUpper contents
