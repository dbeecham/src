import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Source IO Int
source = CL.sourceList [1..4]

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

multiply :: Conduit Int IO Int
multiply = CL.map (* 2)

conduit :: Conduit Int IO String
conduit = CL.map show

main = do
    source $$ multiply =$= conduit =$ sink
