#!/usr/bin/runhaskell
-- sort.hs
-- sorts stats

import System.Directory

putStrNl = putStrLn

main = do
    currentDirectory <- getCurrentDirectory
    putStr "*All* non-hidden .dat files in \""
    putStr currentDirectory
    putStr "\" will be sorted.  Type the name of the directory to continue: \n"
    confirm <- getLine
    if confirm == currentDirectory then
        do
            putStrNl "TODO"
    else
        do
            putStr "You typed \""
            putStr confirm
            putStr "\", which doesn't match \""
            putStr currentDirectory
            putStr "\".  Stopping.\n"
