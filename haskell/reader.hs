-- The reader transformer is great for storing
-- read-only values in a monad.
-- The reader monad is useful for computations which
-- depend on some environment; like, for instance,
-- if computed on a Windows machine or a Linux machine.

import Control.Monad.Reader
import Control.Monad

-- The MyWorld data type is the environment in which
-- the computation is done.
data MyWorld = MyWorld { avalue :: Int
                       , astring :: String
                       , alist :: [Int]
                       }

-- Need an initial world. This is not changed during the
-- computation of a monad (there is no notion of this
-- in the reader monad), though a monad can execute 
-- another computation in another reader monad which can
-- have another environment.
initialWorld :: MyWorld
initialWorld = MyWorld { avalue = 3
                       , astring = "Hello, world."
                       , alist = [0, 2, 4]
                       }

-- Here is an example of a computation which depend on
-- it's environment. It branches on avalue being 0.
r :: ReaderT MyWorld IO Int
r = do
    lift $ putStrLn "Is avalue 0?"
    a <- ask
    if avalue a == 0
        then do
            lift $ putStrLn "Yes it is!"
            return $ sum (alist a)
        else do
            lift $ putStrLn "No, it's not."
            return $ avalue a


main :: IO ()
main = do
    putStrLn "run computation..."
    val <- runReaderT r initialWorld
    putStrLn "done. result: "
    print val
