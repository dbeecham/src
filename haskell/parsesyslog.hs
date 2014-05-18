{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Char8
import Data.Time
import Data.Word
import qualified Data.ByteString as B
import Control.Applicative

data LogEntry = LogEntry {entryTime     :: LocalTime,
                          entryHost     :: String,
                          entryProcName :: String,
                          entryMessage  :: String} deriving Show
type Log = [LogEntry]

monthParser :: String -> Int
monthParser mon
    | mon == "Jan" = 1
    | mon == "Feb" = 2
    | mon == "Mar" = 3
    | mon == "Apr" = 4
    | mon == "May" = 5
    | mon == "Jun" = 6
    | mon == "Jul" = 7
    | mon == "Aug" = 8
    | mon == "Sep" = 9
    | mon == "Oct" = 10
    | mon == "Nov" = 11
    | mon == "Dec" = 12

timeParser :: Parser LocalTime
timeParser = do
    mm <- count 3 anyChar
    space
    d <- count 2 digit
    space
    h <- count 2 digit
    char ':'
    m <- count 2 digit
    char ':'
    s <- count 2 digit
    return LocalTime {
        localDay = fromGregorian 2014 (monthParser mm) (read d),
        localTimeOfDay = TimeOfDay (read h) (read m) (read s)}

wordParser :: Parser String
wordParser = many1 $ satisfy $ not . isSpace

messageParser = many1 $ notChar '\n'

logEntryParser :: Parser LogEntry
logEntryParser = do
    time <- timeParser
    space
    host <- wordParser
    space
    procname <- wordParser
    space
    message <- messageParser
    return $ LogEntry time host procname message

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine
    

main = B.readFile "/var/log/syslog" >>= print . parseOnly logParser
