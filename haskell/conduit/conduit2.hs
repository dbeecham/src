import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Source IO Int
source = do
    yield 1
    yield 2
    yield 3
    yield 4


tostring :: Conduit Int IO String
tostring = CL.map show

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn


main = do
    source $$ tostring =$ sink
