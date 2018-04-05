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
getStatesString (s:[]) = show s
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
  else error "State cannot be defined by an empty line"

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
  print $ reduced
  view $ rebuild reduced complete
  where
    sink = maximum(getStates dfa)+1
    complete = makeComplete dfa sink
    start = [(getEndStates complete), (getStates complete) \\ (getEndStates complete)]
    reduced = zip [1..] $ compareResults [] start complete

rebuild :: [(Int,[Int])] -> DFA -> DFA
rebuild reduced old@(DFA _ start end trans alpha) = DFA {
        states = sort $ map(\x -> fst x) reduced,
        startState = renameState reduced start,
        endStates = sort $nub $ map(\x -> renameState reduced x ) end,
        transitions = sortT $ nub $ renameTransitions reduced trans,
        alphabet = alpha
}

renameTransitions :: [(Int,[Int])] -> [Transition] -> [Transition]
renameTransitions _ [] = []
renameTransitions r ((Transition c s e):xs) =
  (Transition (renameState r c) s (renameState r e) ):renameTransitions r xs

renameState :: [(Int,[Int])] -> Int -> Int
renameState (x:xs) oldState
  | elem oldState (snd x) = fst x
  | otherwise = renameState xs oldState

-- function executes the minimization procces until it yields the same results
-- in two iterations
compareResults :: [[Int]] -> [[Int]] -> DFA -> [[Int]]
compareResults old new dfa
  | old == new = new
  | otherwise = compareResults new (minimize new dfa) dfa

minimize :: [[Int]] -> DFA -> [[Int]]
minimize _ (DFA _ _ _ _ []) = []
minimize states dfa@(DFA allSt start end trans (a:as)) = stepStates minimizer len
  where
    len = length allSt
    nextDFA = (DFA allSt start end trans as)
    new = trace("Splitted with" ++ show a ++": " ++ show states ++ " to: " ++ show (splitItIfYouCan states trans a)) splitItIfYouCan states trans a
    minimizer = (sortByLen $ nub $ sort(map(\x -> sort $ nub x)(new ++ minimize new nextDFA)))

stepStates :: [[Int]] -> Int -> [[Int]]
stepStates [] _ = []
stepStates l@(s:sx) len
  | ((length s) <= len && (len > 0)) = s:(stepStates newXs (len - length s))
  | otherwise = []
    where
      newXs = deleteAdded s sx

deleteAdded :: [Int] -> [[Int]] -> [[Int]]
deleteAdded s [] = []
deleteAdded s (x:xs)
  | ((intersect s x) /= []) = deleteAdded s xs
  | otherwise = x:deleteAdded s xs

-- Test if current allocation of states has to be split and make if required
splitItIfYouCan :: [[Int]] -> [Transition] -> String-> [[Int]]
splitItIfYouCan [] _ _ = []
splitItIfYouCan (state:xs) trans a =
  (split state trans a) ++ splitItIfYouCan xs trans a

split :: [Int] -> [Transition] -> String -> [[Int]]
split currentStates trans a
  | (length currentStates == 1) = trace(show a ++": Checked: " ++ show [currentStates])[currentStates]
  | otherwise = trace (show a ++ " : "++show currentStates ++" Calculated: " ++ show (delete [] [currentClass, currentStates \\ currentClass]) ++ "for: " ++ show endStates) delete [] [currentClass, currentStates \\ currentClass]
    where
      endStates = nub $ filter(\x -> elem x currentStates) $ provideEndStates currentStates trans a
      currentClass = filter(\x -> (leadToEndStates x a endStates trans)) currentStates

leadToEndStates _ _ _ [] = False
leadToEndStates state a endStates ((Transition c s n):xs)
  | ((state == c) && (a == s) && (elem n endStates)) = True
  | otherwise = leadToEndStates state a endStates xs

checkClass :: [Int] -> [[Int]] -> Bool
checkClass _ [] = False
checkClass endStates (l:ls)
  | intersect l endStates == [] = True
  | otherwise = checkClass endStates ls

provideEndStates :: [Int] -> [Transition] -> String -> [Int]
provideEndStates states trans a = makeEndStatesList states trans a

-- Returns list of endStates returned by executing the transition on states from
-- list l with symbol a
makeEndStatesList :: [Int] -> [Transition] -> String -> [Int]
makeEndStatesList [] _ _  = []
makeEndStatesList l@(s:xs) trans a = [makeTransition s a trans] ++ makeEndStatesList xs trans a

makeTransition :: Int -> String -> [Transition] -> Int
makeTransition state symbol ((Transition c s e):xs)
  | state == c && symbol == s = e
  | otherwise = makeTransition state symbol xs

-- Checks if DFA has a transition for every symbol in Σ, if not add SINK state
makeComplete :: DFA -> Int -> DFA
makeComplete dfa@(DFA states start end trans alpha) sink
  | (isComplete dfa) = dfa
  | otherwise = DFA {
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
  | (hasRule state a trans) = checkState state trans as sink
  | otherwise = [(Transition state a sink)] ++ (checkState state trans as sink)

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
  | length (getTransitions dfa) == length (getStates dfa) * length (getAlphabet dfa) = True
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
