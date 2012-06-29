-- Neet better error checking
-- Need a better interface in Utils to handle errors?
--
-- orthanc% cat -n a.txt non1 b.txt non2
--      1  a
-- cat: non1: No such file or directory
--      2  b
-- cat: non2: No such file or directory

-- Flags implemented as "stacked" String -> String filters
module Main(main) where
import Control.Monad.State
import Data.Bits(xor)
import Data.Char(ord)
import Data.List(isPrefixOf)
import System.Environment (getArgs)
import System.Console.GetOpt
import Utils

main = do args <- getArgs
	  let (flags, files, errs) = getOpt Permute opts args
	  let printer = getPrintStyle flags
	  let numberer = getNumberStyle flags
	  let (tabs, ends, squeeze) = reifyFlags flags
	  let filter = getFilter tabs ends squeeze numberer printer
          list <- sequence $ getFileContents files
          let strings = map (either (\_ -> "") id) list
          putStr $ filter $ concat strings

data Flag = ShowAll | NumberNonBlank | E | ShowEnds | Number | Reversible |
            SqueezeBlank | T | U | ShowTabs | ShowNonPrinting deriving Eq

opts :: [OptDescr Flag]
opts = [ Option ['A'] ["show-all"] (NoArg ShowAll) "-vET"
       , Option ['b'] ["number-nonblank"] (NoArg NumberNonBlank) "number non-blank output lines"
       , Option ['e'] [] (NoArg E) "-vE"
       , Option ['E'] ["show-ends"] (NoArg ShowEnds) "display $ at end of each line"
       , Option ['n'] ["number"] (NoArg Number) "number all output lines"
       , Option ['r'] ["reversible"] (NoArg Reversible) "use \\ to make transforms reversible."
       , Option ['s'] ["squeeze-blank"] (NoArg SqueezeBlank) "only print first in series of blank lines."
       , Option ['t'] [] (NoArg T) "-vT"
       , Option ['T'] ["show-tabs"] (NoArg T) "Display TAB characters as ^I"
       , Option ['u'] [] (NoArg U) "(ignored)"
       , Option ['v'] [] (NoArg ShowNonPrinting) "use ^ and M- notation (except LF and TAB)"
       ]

data NonPrintStyle = NoChange | Metafy    | Reversing
data NumberStyle   = NoNums   | AllNums | NonBlankNums

getPrintStyle :: [Flag] -> NonPrintStyle
getPrintStyle flags = if (Reversible `elem` flags)
		      then Reversing
		      else if (or [(ShowAll `elem` flags)
				  ,(E `elem` flags)
				  ,(T `elem` flags)
				  ,(ShowNonPrinting `elem` flags)])
		      then Metafy
		      else NoChange

getNumberStyle :: [Flag] -> NumberStyle
getNumberStyle flags = if (NumberNonBlank `elem` flags)
		       then NonBlankNums
		       else if (Number `elem` flags)
		       then AllNums
		       else NoNums

getFiles :: [String] -> [String]
getFiles args = let (_, files, _) = getOpt Permute opts args in files

getFilter :: Bool -> Bool -> Bool -> NumberStyle -> NonPrintStyle -> (String -> String)
getFilter tabs ends squeeze num print = (
	if (ends)
	then showEndsFilter
	else id
	) . (
	case num of
		NoNums -> id
		AllNums -> numberAll
		NonBlankNums -> numberNonBlank
	) . (
	case print of 
		NoChange -> id
		Reversing -> backslashify
		Metafy -> metafy
	) . (
	if (tabs)
	then showTabsFilter
	else id
	) . (
	if (squeeze)
	then squeezeBlanksFilter
	else id
	)

newlines = '\n':'\n':[]

squeezeBlanksFilter :: String -> String
squeezeBlanksFilter ('\n':rest) | newlines `isPrefixOf` rest  =
				     squeezeBlanksFilter rest
squeezeBlanksFilter (x:xs)         = x : (squeezeBlanksFilter xs)
squeezeBlanksFilter []             = []

reifyFlags :: [Flag] -> (Bool, Bool, Bool)
reifyFlags flags = ( (T `elem` flags) || (ShowTabs `elem` flags)
                   , (E `elem` flags) || (ShowEnds `elem` flags)
		   , (SqueezeBlank `elem` flags)
		   )

leftpad n s = pad ++ s where pad = replicate (n - length s) ' '

numberAll :: String -> String
numberAll x = unlines $ zipWith (\a b -> (leftpad 6 (show a)) ++ "  " ++ b) [1..] $ lines x

numberNonBlank :: String -> String
numberNonBlank = unlines . fst . (\x -> runState (mapM numberLine x) 1) . lines

numberLine :: String -> State Int String
numberLine "" = return ""
numberLine s  = do n <- get
                   put (n+1)
                   return $ (leftpad 6 (show n)) ++ "  " ++ s

metafy :: String -> String
metafy = concatMap (metafy1)

metatable :: [(Char, String)]
metatable = [(toEnum a, control a)      | a <- [0..31] ++ [127], a /= 9, a /= 10] ++
            [(toEnum a, control_meta a) | a <- [128..159]] ++
            [(toEnum a, meta a        ) | a <- [160..255]] where
            control x      = "^"    ++ [(toEnum (x `xor` 64))]
            meta x         = "M-"   ++ [(toEnum (x - 128))]
            control_meta x = "M-^"  ++ [(toEnum (x - 64))]

metafy1 x = case lookup x metatable of Just y  -> y
                                       Nothing -> [x]

-- does metafy, and further escapes '\\', 'M' and '^'
backslashify :: String -> String
backslashify = concatMap (backslashify1)

backslashify1 '\\' = "\\\\"
backslashify1 '^' = "\\^"
backslashify1 'M' = "\\M"
backslashify1 x = metafy1 x

-- bug: file without EOL.
showEndsFilter :: String -> String
showEndsFilter = unlines . map (++ "$") . lines

showTabsFilter :: String -> String
showTabsFilter = concatMap tabreplace where
    tabreplace '\t' = "^I"
    tabreplace x = [x]
