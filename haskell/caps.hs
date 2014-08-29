import Data.Char -- toUpper
import Control.Monad -- forever

main = forever $ do
    l <- getLine
    putStr $ map toUpper l
