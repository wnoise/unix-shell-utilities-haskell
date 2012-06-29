module Main(main) where
import System.Console.GetOpt
import System.Environment (getArgs)
import Data.List
import Utils

-- error checking

main :: IO ()
main = do args <- getArgs
          let (flags, filenames, errs) = getOpt RequireOrder opts args
          let serial = Serial `elem` flags
          let dels = getDelims $ reverse $ filter isDelim flags
          let fun = choose_fun serial
          filecontents <- sequence $ getFileContents filenames
          fun dels $ map lines $ map fromRight filecontents
          return ()
         
data Flag = Serial | Delimiters [Char] deriving Eq

opts :: [OptDescr Flag]
opts = [
     Option ['d'] ["delimeters"] (ReqArg (Delimiters) "DELIM-LIST") "Use these to merge lines."
    ,Option ['s'] ["serial"] (NoArg Serial) "Go through line-by-line of one file at a time."
    ]

getDelims :: [Flag] -> [Char]
getDelims ((Delimiters x):_) = cycle x
getDelims [] = cycle "\t"

isDelim :: Flag -> Bool
isDelim Serial = False
isDelim (Delimiters _) = True

combinewith :: [Char] -> [[String]] -> IO ()
combinewith dels contents | allEmpty contents = return ()
combinewith dels contents | otherwise = combinewith' dels (map head' contents) >>
                                        combinewith  dels (map tail' contents)

combinewith' :: [Char] -> [String] -> IO ()
combinewith' dels [] = error "Must have at least one source file."
combinewith' dels [x] = putStrLn x
combinewith' dels (x:xs@(_:_)) = putStr x >> putStr (take 1 dels) >> combinewith' (tail dels) xs

head' :: [[a]] -> [a]
head' [] = []
head' xs = head xs

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "fromRight"

allEmpty :: [[a]] -> Bool
allEmpty = and . map null

serialize :: [Char] -> [[String]] -> IO ()
serialize dels streams = mapM_ (serializeOne dels) streams

serializeOne :: [Char] -> [String] -> IO ()
serializeOne = combinewith'

choose_fun :: Bool -> ([Char] -> [[String]] -> IO ())
choose_fun True = serialize
choose_fun False = combinewith
