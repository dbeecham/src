import Data.Char -- toUpper

main = do
    contents <- getContents
    putStr $ map toUpper contents

-- From Learn you a Haskell for great good:
-- As you can see, it prints out our capslocked input back to us line by line.
-- When the result of getContents is bound to contents, it's not represented in
-- memory as a real string, but more like a promise that it will produce the
-- string eventually. When we map toUpper over contents, that's also a promise
-- to map that function over the eventual contents. And finally when putStr
-- happens, it says to the previous promise: "Hey, I need a capslocked line!".
-- It doesn't have any lines yet, so it says to contents: "Hey, how about
-- actually getting a line from the terminal?". So that's when getContents
-- actually reads from the terminal and gives a line to the code that asked it
-- to produce something tangible. That code then maps toUpper over that line and
-- gives it to putStr, which prints it. And then, putStr says: "Hey, I need the
-- next line, come on!" and this repeats until there's no more input, which is
-- signified by an end-of-file character.
