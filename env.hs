module Main(main) where
import System
import System.Posix.Env

main = do
       args <- getArgs
       case args of [] -> dump_environment
                    _  -> error "unimplemented"

dump_environment :: IO ()
dump_environment = getEnvironmentPrim >>= (mapM_  putStrLn)
