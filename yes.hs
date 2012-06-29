module Main(main) where
import System.Environment (getArgs)
import Data.List (intersperse)

main = do args <- getArgs
          case args of ["--help"]    -> help
                       ["--version"] -> version
                       []            -> forever $ putStrLn "y"
                       _             -> forever $ putStrLn $ concat $ intersperse " " args

forever :: IO () -> IO ()
forever x = x >> forever x

help :: IO ()
help = 
       putStrLn "Usage: yes [STRING]..."
    >> putStrLn "  or:  yes OPTION"
    >> putStrLn "Repeatedly output a line with all specified STRING(s), or `y'."
    >> putStrLn ""
    >> putStrLn "      --help     display this help and exit"
    >> putStrLn "      --version  output version information and exit"

version :: IO ()
version =
       putStrLn "yes (Haskell educational reimplementation)"
    >> putStrLn "Written by Aaron Denney"
    >> putStrLn "Copyright (C) 2005 Aaron Denney"
