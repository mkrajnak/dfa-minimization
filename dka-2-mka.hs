-- DKA-2-MKA
-- Martin Krajnak, xkrajn02@stud.fit.vutbr.cz

import System.Environment

main = do
    (command:args) <- getArgs
    performCommand command args

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  putStr contents

transform :: [String] -> IO ()
transform [fileName] = do
    contents <- readFile fileName
    putStr contents

performCommand :: String -> [String] -> IO ()
performCommand cmd file
  | cmd == "-i" = view file
  | cmd == "-t" = transform file
  | otherwise = putStrLn "Usage: dka-2-mka [-i|-t] [file]"
