module Main(main) where
import System
import System.Posix.Env

main = do
       args <- getArgs
       handle args

dump_environment :: IO ()
dump_environment = getEnvironmentPrim >>= (mapM_  putStrLn)

handle [] = dump_environment
handle (x:xs) | is_env_setting x = set_env x >> handle xs
handle (x:xs) | is_argument x = handle_arg x >> handle xs
handle xs = execute xs

is_env_setting = any (== '=')

count pred xs = length $ filter pred xs

set_env = putEnv

is_argument "-i" = True
is_argument "--ignore-environment" = True
is_argument "-u" = True
is_argument "--unset=NAME" = True
is_argument "--help" = True
is_argument "--version" = True
is_argument "-" = True

handle_arg = error "handle_arg is stub."

execute xs = error "execute is stub."
