-- DFA-2-MKA, project for FLP course at FIT BUT
-- Martin Krajnak, xkrajn02@stud.fit.vutbr.cz

import System.Environment
import System.IO
import Data.List
import Debug.Trace

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

data Hopcroft = Hopcroft {
        p :: [String],
        w :: [String]
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

getW :: Hopcroft -> [String]
getW (Hopcroft w _) = w

getP :: Hopcroft -> [String]
getP (Hopcroft _ p) = p

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
        transitions = parseTransition xs,        -- the rest of lines are output
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

view :: DFA -> IO()
view dfa = do
  putStrLn $ getStates dfa
  putStrLn $ getStartState dfa
  putStrLn $ getEndStates dfa
  mapM_ putStrLn $ getTransitions dfa


getStatesAsList :: DFA -> [String]
getStatesAsList dfa = getSeparatedSubStrings $ getStates dfa

getEndStatesAsList :: DFA -> [String]
getEndStatesAsList dfa = getSeparatedSubStrings $ getEndStates dfa


start_minimization :: DFA -> IO()
start_minimization dfa = do
  print dfa
  print $ minimize h dfa
  where
    p = sort((getEndStatesAsList dfa) ++ (getStatesAsList dfa \\ getEndStatesAsList dfa))
    w = getEndStatesAsList dfa
    h = Hopcroft { p=p, w=w }


--minimize :: Hopcroft -> DFA -> Hopcroft
minimize h@(Hopcroft _ []) _ = h  -- while W is not empty
minimize (Hopcroft p (state:xs)) dfa = minimize tmp dfa
  where
    tmp = goThroughAlphabet state (Hopcroft {p=p, w=xs}) (getAlphabet dfa) dfa-- let X


--goThroughAlphabet :: String -> Hopcroft -> [String] -> DFA -> Hopcroft
goThroughAlphabet _ h [] _ = h
goThroughAlphabet state h (a:as) dfa =trace ("state: " ++ show state ++ "symbol:" ++ show a ++ " h:" ++ show h ) goThroughAlphabet state tmp as dfa
  where
    tmp = goThroughSetX x h
    x = getAllTransitionsWith state a (getTrans dfa)

--goThroughSetX :: [String] -> Hopcroft -> Hopcroft
goThroughSetX _ old@(Hopcroft [] w) = old
goThroughSetX x old@(Hopcroft (y:ps) w)
  | intersect x [y]  /= [] && [y] \\ x /= [] = trace ("x: " ++ show x ++ " y: " ++ show [y] ++ "\nold: " ++ show old ++ " n: " ++ show Hopcroft {p=(replaceP x y ps), w=(replaceW x y w)}) $ goThroughSetX x (Hopcroft {p=(replaceP x y ps), w=(replaceW x y w)})
  | otherwise = trace ("x: " ++ show x ++ " y: " ++ show [y]) $ goThroughSetX x (Hopcroft {p=ps, w=w})

replaceP :: [String] -> String -> [String] -> [String]
replaceP x y ps = ((intersect [y] x) ++ ([y] \\ x)) ++ ps


replaceW :: [String] -> String -> [String] -> [String]
replaceW x y w
  | elem y w = replaceP x y $ delete y w
  | length(intersect [y] x) <= length([y] \\ x) = (intersect [y] x) ++ w
  | otherwise = ([y] \\ x) ++ w


getAllTransitionsWith :: String -> String -> [Transition] -> [String]
getAllTransitionsWith _ _ [] = []
getAllTransitionsWith state symbol ((Transition c s e):ts)
  | state == c && symbol == s = e:(getAllTransitionsWith state symbol ts)
  | otherwise = getAllTransitionsWith state symbol ts


handleAutomata :: String -> DFA -> IO()
handleAutomata cmd dfa
  | cmd == "-i" = view dfa
  | otherwise = start_minimization dfa


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
        else handleAutomata (head args) (parseDFA lineByLine)
