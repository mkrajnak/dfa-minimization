-- DKA-2-MKA
-- Martin Krajnak, xkrajn02@stud.fit.vutbr.cz

import System.Environment
import System.IO

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

view :: String -> IO()
view dka = do
  putStrLn dka

minimize :: String -> IO ()
minimize dka = do
  putStrLn dka

handleAutomata :: String -> String -> IO()
handleAutomata cmd dka = if cmd == "-i"
  then view dka
  else minimize dka

main = do
  (cmd:args) <- getArgs
  if (not (checkArgs cmd) || length args > 1)
    then error "Usage: dka-2-mka [-i|-t] [file]"
    else do
      automata <- getAutomata (firstArg args)
      handleAutomata cmd automata
