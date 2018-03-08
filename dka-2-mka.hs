-- DFA-2-MKA, project for FLP course at FIT BUT
-- Martin Krajnak, xkrajn02@stud.fit.vutbr.cz

import System.Environment
import System.IO

data Transition = Transition {
        currState:: String,
        symbol :: String,
        nextState :: String
} deriving Show

data DFA = DFA {
        states :: String,
        startState :: String,
        endStates :: String,
        transitions :: [Transition]
} deriving Show

getStates :: DFA -> String
getStates (DFA states _ _ _) = states

getStartState :: DFA -> String
getStartState (DFA _ startState _ _) = startState

getEndState :: DFA -> String
getEndState (DFA _ _ endStates _) = endStates

getTransitions :: DFA -> [String]
getTransitions (DFA _ _ _ t) = getTransition t

getTransition :: [Transition] -> [String]
getTransition [] = []
getTransition ((Transition c s n):ts) = (c++","++s++","++n):getTransition ts

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

parseDFA :: [String] -> DFA
parseDFA (f:s:t:xs) = DFA {
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

getFileArg :: [String] -> String
getFileArg x = if (length x == 2)
  then x !! 1
  else ""

checkArgs :: String -> Bool
checkArgs arg = isInList arg ["-i", "-t"]

getAutomata :: String -> IO String
getAutomata fileName = do
  if fileName == ""
    then getContents
    else readFile fileName

view :: [String] -> IO()
view raw_dfa = do
  putStrLn $ getStates dfa
  putStrLn $ getStartState dfa
  putStrLn $ getEndState dfa
  mapM_ putStrLn $ getTransitions dfa
  where dfa = parseDFA raw_dfa

minimize :: [String] -> IO ()
minimize raw_dfa = do
  print $ parseDFA raw_dfa

handleAutomata :: String -> [String] -> IO()
handleAutomata cmd dfa = if cmd == "-i"
    then view dfa
    else minimize dfa

-- check if list has at least num lines
hasNumLines :: Int -> [String] -> Bool
hasNumLines num list = num > (length list)

getHelp :: String
getHelp = "Usage: ./dfa-2-mka [-i|-t] [file]"

main = do
  args <- getArgs
  -- check argument 1. zero args 2. 1st must be in [-i|-t] 3. less than tree
  if ((length args == 0) || not (checkArgs $ head args) || (length args) > 2 )
    then error getHelp
    else do
      automata <- getAutomata $ getFileArg args       -- break input lineByLine
      let lineByLine = lines automata
      if hasNumLines 4 lineByLine         -- input should have at least 4 lines
        then error "Automata description too short"
        else handleAutomata (head args) lineByLine
