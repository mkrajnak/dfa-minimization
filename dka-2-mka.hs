-- DKA-2-MKA
-- Martin Krajnak, xkrajn02@stud.fit.vutbr.cz

import System.Environment

main = do
  getArgs >>= parse

parse ["-i"] = putStrLn("-i")
parse ["-t"] = putStrLn("-t")
parse [] = putStrLn("Usage: dka-2-mka [-i|-t] [file|stdin]")
