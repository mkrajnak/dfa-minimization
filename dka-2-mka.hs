-- DKA-2-MKA
-- Martin Krajnak, xkrajn02@stud.fit.vutbr.cz

import System.Environment
import System.IO

data Transition = Transition {
        currState:: String,
        symbol :: String,
        nextState :: String
} deriving Show

data DKA = DKA {
        states :: String,
        startState :: String,
        endStates :: String,
        transitions :: [Transition]
} deriving Show

getStringToDelim :: Char -> String -> String
getStringToDelim _ [] = []
getStringToDelim d (x:xs)
          | x == d = []
          | otherwise = x : getStringToDelim d xs

jumpToDelim :: Char -> String -> [String]
jumpToDelim _ [] = []
jumpToDelim d (x:xs)
          | x == d = getSeparatedSubStrings xs
          | otherwise = jumpToDelim d xs

getSeparatedSubStrings :: String -> [String]
getSeparatedSubStrings [] = []
getSeparatedSubStrings (x:xs) = getStringToDelim ',' (x:xs) : (jumpToDelim ',' xs)

parseTransition :: [String] -> [Transition]
parseTransition [] = []
parseTransition (x:xs) = Transition {
        currState =  (getSeparatedSubStrings x) !! 0,
        symbol = (getSeparatedSubStrings x) !! 1,
        nextState = (getSeparatedSubStrings x) !! 2
  } : parseTransition xs

parseDKA :: [String] -> DKA
parseDKA (f:s:t:xs) = DKA {
        states = f,                     -- states are declared on the first line
        startState = s,           -- start state are declared on the second line
        endStates = t,              -- end states are declared on the third line
        transitions = parseTransition xs       -- the rest of lines are output
}

isInList :: Eq a => a -> [a] -> Bool
isInList _ [] = False
isInList el (x:xs) = do
  if el == x
    then True
    else isInList el xs

firstArg :: [String] -> String
firstArg [] = ""
firstArg (x:xs) = x

checkArgs :: String -> Bool
checkArgs arg = isInList arg ["-i", "-t"]

getAutomata :: String -> IO String
getAutomata fileName = do
  if fileName == ""
    then getContents
    else readFile fileName

view :: [String] -> IO()
view dka = do
  print $ parseDKA dka

minimize :: [String] -> IO ()
minimize dka = do
  print dka

handleAutomata :: String -> [String] -> IO()
handleAutomata cmd dka = if cmd == "-i"
    then view dka
    else minimize dka

-- check if list has at least num lines
hasNumLines :: Int -> [String] -> Bool
hasNumLines num list = num > (length list)


main = do
  (cmd:args) <- getArgs
  if (not (checkArgs cmd) || length args > 1)
    then error "Usage: dka-2-mka [-i|-t] [file]"
    else do
      automata <- getAutomata $ firstArg args -- break input lineByLine
      let lineByLine = lines automata
      print $ length lineByLine
      print $ getSeparatedSubStrings "One,Two,Tree"
      if hasNumLines 4 lineByLine         -- input should have at least 4 lines
        then error "Automata description too short"
        else handleAutomata cmd lineByLine
