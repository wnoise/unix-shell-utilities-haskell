module Main(main) where
import System
import System.Console.GetOpt
import Data.Char (chr, isOctDigit, digitToInt) 
-- Two common echos:
-- (SysV) -n skips newline,
-- (BSD)  \c in string skips newline, when escapes interpreted.

main = do args <- getArgs
          let (interpret, skip, remains, errs) = get_flags args
          let (reallyskip, string) = if interpret then
	                             rewrite_string skip (unwords remains)
				     else (skip, unwords remains)
          putStr string
          if not reallyskip then putStrLn "" else return ()

escapetable '\\' = "\\"
-- escapetable '\number' = octal value
escapetable 'a' = "\a"
escapetable 'b' = "\b"
-- escapetable 'c' = suppress newline
escapetable 'f' = "\f"
escapetable 'n' = "\n"
escapetable 'r' = "\r"
escapetable 't' = "\t"
escapetable 'v' = "\v"
escapetable  x  = '\\':x:[]

data Flag = Interpret | SkipNewline deriving (Show, Eq)
options :: [OptDescr Flag]
options = [ Option ['n'] [] (NoArg SkipNewline) "do not output newline"
          , Option ['e'] [] (NoArg Interpret) "Interpret escape sequences"
          ]

get_flags argv = case getOpt RequireOrder options argv of
               (opts, remains, errs) ->
                   let interpret = Interpret `elem` opts
                       skip = SkipNewline `elem` opts
                    in (interpret, skip, remains, errs)

-- Should stick in state monad or something, for efficiency.
rewrite_string :: Bool -> String -> (Bool, String)
rewrite_string skip ('\\':'c':rest) = rewrite_string True rest
rewrite_string skip ('\\':[])       = (skip, "\\")
rewrite_string skip ('\\':rest)     = let (int, more) = string_escape rest
                                          (skip', cont) = rewrite_string skip more
					in (skip', int ++ cont)
rewrite_string skip (x:xs)          = let (skip', cont) = rewrite_string skip xs
					in (skip', x:cont)
rewrite_string skip []              = (skip, [])


string_escape :: String -> (String, String)
string_escape [] = ([],[])
string_escape x  = if isOctDigit (head x)
                   then let (value, rest) = break isOctDigit x
			in  ([(chr (fromOctal value))], rest) 
                   else (escapetable (head x), tail x)

octalValue x | isOctDigit x = digitToInt x
octalValue err = error $ "octalValue called with " ++ [err] ++ "."

fromOctal :: String -> Int
fromOctal s = fromOctal'  0  s where
              fromOctal' acc [] = acc
              fromOctal' acc (x:xs)  = fromOctal' (8*acc + octalValue x) xs
