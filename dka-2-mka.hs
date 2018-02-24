-- DKA-2-MKA
-- Martin Krajnak, xkrajn02@stud.fit.vutbr.cz


import System.Environment
import System.IO

-- main = do
--   putStrLn "Type name"
--   name <- getLine
--   putStrLn ("Hello " ++ name)
--   getChar

main = getArgs >>= parse

parse ["-i"] = putStrLn("-i")
parse ["-t"] = putStrLn("-t")
