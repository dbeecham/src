-- The writer monad is useful for keeping track of some
-- 'byproduct' of a computation; a byproduct which the
-- computation itself does not depend on.
-- This file uses a log as an example.

import Control.Monad.Writer

w0 :: WriterT String IO ()
w0 = do
    tell "Inside w0."
    tell "Printing string."
    lift $ putStrLn "Hello, world"
    tell "Done."
    tell "Running another writer monad."
    listen w1
    tell "Done."
    tell "Returning."
    return ()

w1 :: WriterT String IO ()
w1 = do
    tell "Inside w1."
    tell "Doing some calculations."
    lift $ putStrLn $ show $ 3 + 100
    tell "Done."
    tell "Returning."
    return ()

main :: IO ()
main = do
    (out, log) <- runWriterT w0
    putStrLn "Ran execution!"
    putStrLn $ "Log is: " ++ log
    return ()
