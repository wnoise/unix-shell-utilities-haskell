module Utils (
              getFileContents,
              getFileHandles,
              intersperse_error,
              join,
              split_either
              ) where
import IO
import Data.List (intersperse)
-- import Either

getFileContents :: [String] -> [IO (Either IOError String)]
getFileContents [] = [getContents >>= return.Right]
getFileContents xs = map getOneFile xs

getFileHandles :: [String] -> [IO (Either IOError Handle)]
getFileHandles [] = [return (Right stdin)]
getFileHandles xs = map getOneHandle xs

getOneFile :: String -> IO (Either IOError String)
getOneFile "-" = getContents >>= return . Right
getOneFile  x  = withFile x (hGetContents)

getOneHandle :: String -> IO (Either IOError Handle)
getOneHandle "-" = return (Right stdin)
getOneHandle  x  = withFile x (return . id)

withFile f a = try (do h <- openFile f ReadMode
                       rv <- a h
                       return rv)

-- Error strategies --
intersperse_error handler normal = mapM_ $ either handler normal
errors_first handler normal xs = let (errs, normals) = split_either xs in
                                     do mapM_ handler errs
                                        mapM_ normal normals
{-
abort_first handler normal xs = let split = split_either xs in
                    case split of (err:_,_) -> handler err
                                  (_,norms) -> mapM_ normal norms
-}

-- Perl's join operator, more or less.
join = (concat .) . intersperse

split_either xs = split_either' ([],[]) xs

split_either' (ls,rs) [] = (reverse ls, reverse rs)
split_either' (ls,rs) ((Left x):xs) = split_either' (x:ls, rs) xs
split_either' (ls,rs) ((Right x):xs) = split_either' (ls, x:rs) xs
