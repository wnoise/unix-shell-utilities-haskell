module Main where
import System.Console.GetOpt
import System(getArgs)
import Data.List
import Utils

-- Error handling

main :: IO ()
main = do args <- getArgs
          let (flags, filenames, errs) = getOpt RequireOrder opts args
          let exceptional = (Help `elem` flags) || (Version `elem` flags) || (length errs > 0)
          let delim = last $ '\t' : (concatMap extractDelim flags)
          let outputdelim = last $ [delim] : (concatMap extractOutDelim flags)
          let dis = (disassemble delim)
          let ass = (assemble outputdelim)
          let sel = (select)
          filecontents <- sequence $ getFileContents filenames
          putStr $ unlines $ map (ass.sel.dis) $ concatMap (lines.fromRight) filecontents
          return ()
         
data Range = UL Int Int | Geq Int | Leq Int deriving Eq
data Flag = Bytes String | Chars String | Fields String
          | Delimiter String | Ignored | OnlyDelimited | OutputDelimiter String
          | Help | Version deriving (Eq, Show)

opts :: [OptDescr Flag]
opts = [
     Option ['b'] ["bytes"] (ReqArg (Bytes) "LIST") "Output only these bytes."
    ,Option ['c'] ["characters"] (ReqArg (Chars) "LIST") "Output only these characters."
    ,Option ['d'] ["delimiter"] (ReqArg (Delimiter)  "DELIM") "Use DELIM instead of TAB for field delimeter"
    ,Option ['f'] ["fields"] (ReqArg (Fields) "LIST") "Output only these fields."
    ,Option ['n'] [] (NoArg (Ignored)) "Ignored for compatibility."
    ,Option ['s'] ["only-delimited"] (NoArg (OnlyDelimited)) "do not print lines not containing delimiters."
    ,Option ['s'] ["output-delimiter"] (ReqArg (OutputDelimiter) "STRING") "use STRING as the output delimiter, the default is to use the input delimiter."
    ,Option []    ["help"] (NoArg Help) "display this help and exit"
    ,Option []    ["version"] (NoArg Version) "output version information and exit"
    ]

instance Show Range where
    show (UL a b) | a == b    = show a
                  | otherwise = (show a) ++ "-" ++ (show b)
    show (Leq a) = "-" ++ (show a)
    show (Geq a) = (show a) ++ "-"

instance Read Range where
    readsPrec _ = rangereads
    readList = listnobracket

-- Can't be empty.  That's okay.
listnobracket :: (Read a) => String -> [([a],String)]
listnobracket str = do (p,rest) <- reads str
                       case rest of (',':xs) -> do (tail, unparsed) <- listnobracket xs
                                                   return (p:tail, unparsed)
                                    []       -> return ([p], "")
                                    _        -> fail "no parse"

rangereads ('-':s) = map (\(a, b) -> (Geq a, b)) (reads s)
rangereads s     = concatMap readrest $ reads s

readrest :: (Int, String) -> [(Range, String)]
readrest (l,'-':s)  = let parses = reads s in
                          if ((length parses) == 0)
                          then [(Geq l, s)]
                          else map (\(a, b) -> (UL l a, b)) $ parses
readrest (l, s)     = [(UL l l, s)]

extractDelim :: (Monad m) => Flag -> m Char
extractDelim (Delimiter (x:_)) = return x
extractDelim _             = fail ""

extractOutDelim :: (Monad m) => Flag -> m String
extractOutDelim (OutputDelimiter x) = return x
extractOutDelim _             = fail ""

fromRight           :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "fromRight"

{-
select :: Range -> [String] -> [String]
select r = (filter ((inrange r).fst)).(zipWith [1..])
-}
select = error "select stub"

disassemble :: Char -> String -> [String]
disassemble = error "stub"

assemble  :: String -> [String] -> String
assemble = (concat .) . intersperse
