-- The state monad transformer is great for
-- keeping track of a state which can change during
-- a computation. It's sort of a combination of the reader
-- monad and the writer monad; it's a computation which
-- depend on the enviromnent it's run on (the state),
-- and it produces an "extra value" (the next state).
-- This file uses a cli as an example.

import Control.Monad.State
import System.IO

-- MyState is the state which the state transformer
-- "hides". The state type can be any type.
data MyState = MyState { avalue :: Int
                       , alist :: [Int]
                       , astring :: String
                       }

-- We need an initial state
initialState :: MyState
initialState = MyState { avalue = 3
                       , alist = [0, 2, 4]
                       , astring = "Hello, world."
                       }

-- As an illustration, let's create a CLI.
-- The CLI is a sort of loop; it reads a command,
-- parses it, runs some function depending on the
-- command, and loops (unless command is 'quit').
loop :: StateT MyState IO ()
loop = do
    lift $ putStr "> "
    lift $ hFlush stdout
    input <- lift getLine
    perform input

perform :: String -> StateT MyState IO ()

-- Quit the cli. Does not call loop.
perform "quit" = do
    return ()

perform "help" = do
    lift $ putStrLn "Avaliable commands are:"
    lift $ putStrLn "quit, help, avalue, alist, astring, inc, add"
    loop

perform "avalue" = do
    -- Store the current state in a
    a <- get
    -- Print out 'avalue' in state
    lift $ putStrLn $ "avalue is " ++ (show (avalue a))
    -- then loop
    loop

perform "alist" = do
    a <- get
    lift $ putStrLn $ "alist is " ++ (show (alist a))
    loop

perform "astring" = do
    a <- get
    lift $ putStrLn $ "astring is " ++ (astring a)
    loop

perform "inc" = do
    a <- get
    -- Store a modified state using put.
    put $ a { avalue = (avalue a) + 1}
    loop

perform "add" = do
    a <- get
    put $ a { alist = 0 : (alist a)}
    loop

perform _ = loop


main :: IO ()
main = do
    -- Run state monad transformer 'loop' with
    -- initial state 'initialState'. Return a tuple
    -- where first value is return value, and s is
    -- state.
    (r, s) <- runStateT loop initialState
    return r
