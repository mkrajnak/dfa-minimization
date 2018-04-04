-- DFA-2-MKA, project for FLP course at FIT BUT
-- Martin Krajnak, xkrajn02@stud.fit.vutbr.cz

import System.Environment
import System.IO
import Data.List
import Debug.Trace

data Transition = Transition {
        currState:: Int,
        symbol :: String,
        nextState :: Int
} deriving (Show, Eq, Ord)

data DFA = DFA { -- Deterministiv Finite State Automata - internal representation
        states :: [Int],
        startState :: Int,
        endStates :: [Int],
        transitions :: [Transition],
        alphabet :: [String]
} deriving Show


view :: DFA -> IO()
view dfa = do
  putStrLn (getStatesString (getStates dfa))
  putStrLn (show (getStartState dfa))
  putStrLn (getStatesString (getEndStates dfa))
  mapM_ putStrLn $ getTransitions dfa

getStates :: DFA -> [Int]
getStates (DFA states _ _ _ _) = states

getStatesString :: [Int] -> String
getStatesString [] = ""
getStatesString (s:xs) = show s ++ "," ++ getStatesString xs

getStartState :: DFA -> Int
getStartState (DFA _ startState _ _ _) = startState

getEndStates :: DFA -> [Int]
getEndStates (DFA _ _ endStates _ _) = endStates

getTrans :: DFA -> [Transition]
getTrans (DFA _ _ _ t _) = t

-- Returns the list of all transitions as a list of Strings (1 transition per line)
getTransitions :: DFA -> [String]
getTransitions (DFA _ _ _ [] _) = []
getTransitions (DFA as st e ((Transition c s n):ts) a) =
  (show c++","++s++","++ show n):getTransitions (DFA as st e (ts) a)

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
        currState = read ((getSeparatedSubStrings x) !! 0) :: Int,
        symbol = (getSeparatedSubStrings x) !! 1,
        nextState = read ((getSeparatedSubStrings x) !! 2) :: Int
  } : parseTransition xs
  else error "State cannot be defined by an empty line" -- TODO rethink if this condition is needed

parseAlphabet :: [String] -> [String]
parseAlphabet [] = []
parseAlphabet (x:xs) = getSymbol x:parseAlphabet xs

getSymbol :: String -> String
getSymbol transition = (getSeparatedSubStrings transition) !! 1

parseDFA :: [String] -> DFA
parseDFA (f:s:t:xs) = DFA {
        states = map(\x -> read x :: Int)(getSeparatedSubStrings f),              -- states are declared on the first line
        startState = read s :: Int,                       -- start state are declared on the second line
        endStates = map(\x -> read x :: Int)(getSeparatedSubStrings t),         -- end states are declared on the third line
        transitions = parseTransition xs,                 -- the rest of lines are output
        alphabet = sort $ nub $ parseAlphabet xs
}

getFileArg :: [String] -> String
getFileArg x = if (length x == 2)
  then x !! 1
  else ""

checkArgs :: String -> Bool
checkArgs arg = elem arg ["-i", "-t"]

-- Reads DFA either from the specified file or stdin
getAutomata :: String -> IO String
getAutomata fileName = do
  if fileName == ""
    then getContents
    else readFile fileName

sortByLen :: [[a]] -> [[a]]
sortByLen l = sortBy(\a b -> compare(length a) (length b)) l

sortT :: [Transition] -> [Transition]
sortT l = sortBy(\a b -> compare a b) l

start_minimization :: DFA -> IO()
start_minimization dfa = do
  print $ complete
  print $ reduced
  view $ rebuild reduced complete
  where
    sink = maximum(getStates dfa)+1
    complete = makeComplete dfa sink
    start = [(getEndStates complete), (getStates complete) \\ (getEndStates complete)]
    reduced = compareResults [] start complete


rebuild :: [[Int]] -> DFA -> DFA
rebuild reduced old@(DFA allSt start end trans alpha) = DFA {
        states = map(\x -> head x) reduced,
        startState = renameState reduced start,
        endStates = nub $ map(\x -> renameState reduced x ) end,
        transitions = nub $ renameTransitions reduced trans,
        alphabet = alpha
}

renameTransitions :: [[Int]] -> [Transition] -> [Transition]
renameTransitions _ [] = []
renameTransitions r ((Transition c s e):xs) =
  (Transition (renameState r c) s (renameState r e) ):renameTransitions r xs

renameState :: [[Int]] -> Int -> Int
renameState (x:xs) oldState
  | elem oldState x = head x
  | otherwise = renameState xs oldState


-- function executes the minimization procces until it yields the same results
-- in two iterations
compareResults :: [[Int]] -> [[Int]] -> DFA -> [[Int]]
compareResults old new dfa
  | old == new = new
  | otherwise = trace("old: " ++ show old ++ " new: " ++ show new) compareResults new (minimize new dfa) dfa

minimize :: [[Int]] -> DFA -> [[Int]]
minimize _ (DFA _ _ _ _ []) = []
minimize states dfa@(DFA allSt start end trans (a:as)) = stepStates minimizer len
  where
    len = length allSt
    nextDFA = (DFA allSt start end trans as)
    new = trace ("split: " ++ show (splitItIfYouCan states trans a))splitItIfYouCan states trans a
    minimizer = (sortByLen $ nub $ sort(map(\x -> sort x)(new ++ minimize new nextDFA)))

stepStates :: [[Int]] -> Int -> [[Int]]
stepStates [] _ = []
stepStates l@(s:sx) len
  | ((length s) <= len && (len > 0)) = trace("L: " ++ show l ++ "adding: " ++ show s ++ " len: " ++ show len ++ " new: " ++ show ((len - length s))) s:(stepStates sx (len - length s))
  | otherwise = []

-- Test if current allocation of states has to be split and make if required
splitItIfYouCan :: [[Int]] -> [Transition] -> String-> [[Int]]
splitItIfYouCan [] _ _ = []
splitItIfYouCan (state:xs) trans a = split state (provideEndStates state trans a) ++ splitItIfYouCan xs trans a

split :: [Int] -> [Int] -> [[Int]]
split [] endStates = []
split currentStates endStates
  | (currentClass == endStates) || (length currentStates == 1) = [currentStates]
  | otherwise = trace("splitting: " ++ show currentClass ++ " by " ++ show endStates ++ " to " ++show(delete [] [currentClass, currentStates \\ currentClass])) delete [] [currentClass, currentStates \\ currentClass]
    where
      currentClass = filter (\x -> elem x currentStates) endStates

checkClass :: [Int] -> [[Int]] -> Bool
checkClass _ [] = False
checkClass endStates (l:ls)
  | intersect l endStates == [] = True
  | otherwise = checkClass endStates ls

provideEndStates :: [Int] -> [Transition] -> String -> [Int]
provideEndStates states trans a = trace("ENDSS: " ++ show (makeEndStatesList states trans a)) makeEndStatesList states trans a

-- Returns list of endStates returned by executing the transition on states from
-- list l with symbol a
makeEndStatesList :: [Int] -> [Transition] -> String -> [Int]
makeEndStatesList [] _ _  = []
makeEndStatesList l@(s:xs) trans a = [makeTransition s a trans] ++ makeEndStatesList xs trans a

makeTransition :: Int -> String -> [Transition] -> Int
makeTransition state symbol ((Transition c s e):xs)
  | state == c && symbol == s = trace ("checking: " ++ show c ++ " symbol: " ++ show s ++ " = " ++ show e) e
  | otherwise = makeTransition state symbol xs

-- Checks if DFA has a transition for every symbol in Σ, if not add SINK state
makeComplete :: DFA -> Int -> DFA
makeComplete dfa@(DFA states start end trans alpha) sink
  | (isComplete dfa) = dfa
  | otherwise = trace("DFA not complete") DFA {
        states = states ++ [sink],
        startState = start,
        endStates = end,
        transitions = sortT(trans ++ addMissingTrans(states ++ [sink]) trans alpha sink),
        alphabet = alpha
}

-- Go through every state and adds a transition to the SINK state if missing
addMissingTrans :: [Int] -> [Transition] -> [String] -> Int -> [Transition]
addMissingTrans [] _ _ _  = []
addMissingTrans (s:ss) trans alpha sink
  = checkState s trans alpha sink ++ addMissingTrans ss trans alpha sink

-- Checks if given state has transition with symbol "a" from Σ, if not add it
checkState :: Int -> [Transition] -> [String] -> Int -> [Transition]
checkState _ _ [] _ = []
checkState state trans (a:as) sink
  | (hasRule state a trans) =  trace("Checking: "++ show state ++ "->"  ++ show a) checkState state trans as sink
  | otherwise = trace ("Adding: " ++ show (Transition state a sink)) [(Transition state a sink)] ++ (checkState state trans as sink)

-- helper function checks if transition is in list of transitions
hasRule :: Int -> String -> [Transition] -> Bool
hasRule _ _ [] = False
hasRule state symbol (t:ts)
  | (hasState state symbol t) = True
  |  otherwise = hasRule state symbol ts

hasState :: Int -> String -> Transition -> Bool
hasState state symbol (Transition c s n)
  | c == state && s == symbol = True
  | otherwise = False

isComplete :: DFA -> Bool
isComplete dfa
  | length (getTransitions dfa) == length (getStates dfa) * length (getAlphabet dfa) = trace("trans: " ++ show (length (getTransitions dfa)) ++ " calc: " ++ show (length (getStates dfa) * length (getAlphabet dfa))) True
  | otherwise = False

-- decite beetween printing or minimization
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
