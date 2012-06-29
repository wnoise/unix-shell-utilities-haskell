clone of "columns" program.

     column [-tx] [-c columns] [-s sep] [file ...]

does not implement -t (and hence -s)  (would ignore -c, -x in that case...)
file reading broken, only stdin now
by necessity is a sponge

> module Main(main) where
> import System.Environment (getArgs, getEnv)
> import System.Console.GetOpt
> import System.IO
> import System.IO.Error (ioeGetErrorString)
> import Utils

Arg parsing

> data Flag = Cols String | Transpose deriving (Show, Eq)
> options :: [OptDescr Flag]
> options = [
>  -- Option ['s'] [] (ReqArg Seper "sep")  "Use sep as seperator",
>  -- Option ['t'] [] (NoArg Retable)       "formats columns together",
>  Option ['x'] [] (NoArg Transpose)       "By line rather than by column",
>  Option ['c'] [] (ReqArg Cols "columns") "Format for width of columns"
>  ]

> header = "Usage: column [-x] [-c columns] [file ...]"
>  -- ++ "\n       column [-t [-s sep]]     [file ...]"
>  -- ++ "\n(second form and files not implemented.)"

> get_flags argv = do
>              case getOpt Permute options argv of
>               (opts, args, []) -> do
>                    let transpose = Transpose `elem` opts
>                    swidth <- getScreenWidth $ reverse opts
>                    return $ Left (transpose, swidth, args)
>               (_, _, errs) -> return $ Right errs

get_flags argv = return $ Left (False, 80, argv)

> main = do
>     argv  <- getArgs
>     flags <- get_flags argv
>     case flags of 
>         Left (do_trans, swidth, args) -> do cont <- concatFiles args
>                                             engine do_trans swidth cont
>         Right errs -> hPutStrLn stderr $ concat errs ++ header 

Right errs -> hPutStrLn stderr $ concat errs ++ usageInfo header options

Engine:

> engine :: Bool -> Int -> String -> IO ()
> engine do_trans swidth input = do
>     let in_lines = lines input
>     let input_width = width in_lines
>     let num_lines = length in_lines
>     let columns = max 1 (swidth `div` (input_width + 1))
>     let rows = (num_lines + columns - 1) `div` columns
>     let result = if (do_trans)
>                  then (xgroupby columns in_lines)
>                  else (groupby  rows    in_lines)
>     putStr $ unlines $ glue $ pad result

Utility:

> width :: [[a]] -> Int
> width = maximum . map length

BUG: Should fall back to stty setting first, rather than environment.
Not exposed in posix mappings even though almost every unix has it.
I.E: command line, stty, environment, "80"

> getScreenWidth [] = catch (getEnv "COLUMNS" >>= return . read)
>                           (\_ -> return 80)
> getScreenWidth ((Cols x):_) = return $ read x
> getScreenWidth (_:xs) = getScreenWidth xs

> takeevery _ _ [] = []
> takeevery n 0 (x:xs) = x:(takeevery n (n-1) xs)
> takeevery n o (_:xs) = (takeevery n (o-1) xs)

> xgroupby n list = [takeevery n i list | i <- [0..n-1]]

> groupby n list = reverse $ groupby' [] list where
>     groupby' acc [] = acc
>     groupby' acc xs = groupby' ((take n xs):acc) (drop n xs)

list of columns -> list of padded columns

> pad :: [[String]] -> [[String]]
> pad matrix = zipWith ($) (map map padtos) matrix
>     where padtos = map padto widths
>           widths = map width matrix

> padto :: Int -> String -> String
> padto w string = string ++ replicate (w + 1 - length string) ' '

> glue :: [[String]] -> [String]
> glue matrix = if width matrix > 0
>               then (concat $ map head' matrix) : (glue $ map tail' matrix)
>               else []
>      where head' [] = []; head' (x:_) = x
>            tail' [] = []; tail' (_:xs) = xs

> concatFiles :: [String] -> IO String
> concatFiles args = do
>                    let contents = getFileContents args
>                    let s = map (\x ->
>                             do x >>= (either
>                                 (elimErrors)
>                                 (return))) contents
>                    sequence s >>= return.concat

> elimErrors :: (IOError, FilePath) -> IO String
> elimErrors (e,f) = writeErr (f ++ ": " ++ (ioeGetErrorString e)) >> return ""

> writeErr :: String -> IO()
> writeErr s = hPutStrLn stderr $ "column: " ++ s

todo: don't pad last column
      investigate using tabs, as original does (and padding to %8...)
      implement -t -s
      empty files fail
      make file reading errors actually give correct answer...
      Should be using some other exception mechanism?
      investigate trying more columns than "should" fit, due to unmatched sizes.
      investigate using equal widths, as original does when not -t.
      last two conflict...
