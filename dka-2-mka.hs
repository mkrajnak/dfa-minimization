-- DFA-2-MKA, project for FLP course at FIT BUT
-- Martin Krajnak, xkrajn02@stud.fit.vutbr.cz

import System.Environment
import System.IO
import Data.List

data Transition = Transition {
        currState:: String,
        symbol :: String,
        nextState :: String
} deriving Show

data DFA = DFA {
        states :: String,
        startState :: String,
        endStates :: String,
        transitions :: [Transition],
        alphabet :: [String]
} deriving Show

getStates :: DFA -> String
getStates (DFA states _ _ _ _) = states

getStartState :: DFA -> String
getStartState (DFA _ startState _ _ _) = startState

getEndStates :: DFA -> String
getEndStates (DFA _ _ endStates _ _) = endStates

getTrans :: DFA -> [Transition]
getTrans (DFA _ _ _ t _) = t

getTransitions :: DFA -> [String]
getTransitions (DFA _ _ _ t _) = getTransition t

getTransition :: [Transition] -> [String]
getTransition [] = []
getTransition ((Transition c s n):ts) = (c++","++s++","++n):getTransition ts

getAlphabet :: DFA -> [String]
getAlphabet (DFA _ _ _ _ alphabet) = alphabet

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
parseTransition (x:xs) = if length x > 0
  then Transition {
        currState =  (getSeparatedSubStrings x) !! 0,
        symbol = (getSeparatedSubStrings x) !! 1,
        nextState = (getSeparatedSubStrings x) !! 2
  } : parseTransition xs
  else error "State cannot be defined by an empty line" -- TODO rethink if this condition is needed

parseAlphabet :: [String] -> [String]
parseAlphabet [] = []
parseAlphabet (x:xs) = getSymbol x:parseAlphabet xs

getSymbol :: String -> String
getSymbol transition = (getSeparatedSubStrings transition) !! 1

unique :: [String] -> [String]
unique [] = []
unique (x:xs)
  | elem x xs = unique xs
  | otherwise = x : unique xs

parseDFA :: [String] -> DFA
parseDFA (f:s:t:xs) = DFA {
        states = f,                     -- states are declared on the first line
        startState = s,           -- start state are declared on the second line
        endStates = t,              -- end states are declared on the third line
        transitions = parseTransition xs,          -- the rest of lines are output
        alphabet = sort $ unique $ parseAlphabet xs
}

isInList :: Eq a => a -> [a] -> Bool
isInList _ [] = False
isInList element (x:xs) = do
  if element == x
    then True
    else isInList element xs

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
  putStrLn $ getEndStates dfa
  mapM_ putStrLn $ getTransitions dfa
  where dfa = parseDFA raw_dfa


getStatesAsList :: DFA -> [String]
getStatesAsList dfa = getSeparatedSubStrings $ getStates dfa

getEndStatesAsList :: DFA -> [String]
getEndStatesAsList dfa = getSeparatedSubStrings $ getEndStates dfa


init_minimization :: [String] -> IO ()
init_minimization raw_dfa = do
  print partition
  print work
  print dfa
  print $ minimize partition work dfa
  where
    dfa = parseDFA raw_dfa
    partition = (getEndStatesAsList dfa) ++ (getStatesAsList dfa \\ getEndStatesAsList dfa)
    work = getEndStatesAsList dfa

--minimize :: [String] -> [String] -> [String]
minimize _ [] _ = []
minimize p w dfa = goThroughAlphabet state alphabet transitions
  where
    state = head w
    alphabet = getAlphabet dfa
    transitions = getTrans dfa


goThroughAlphabet :: String -> [String] -> [Transition]
goThroughAlphabet _ [] _ = []
goThroughAlphabet state (a:as) transitions
  (getAllTransitionsWith state a transitions):(goThroughAlphabet state as transitions)

getAllTransitionsWith :: String -> String -> [Transition] -> [String]
getAllTransitionsWith _ _ [] = []
getAllTransitionsWith state symbol ((Transition c s e):ts)
  | state == c && symbol == s = e:(getAllTransitionsWith state symbol ts)
  | otherwise = getAllTransitionsWith state symbol ts

handleAutomata :: String -> [String] -> IO()
handleAutomata cmd dfa = if cmd == "-i"
    then view dfa
    else init_minimization dfa

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
