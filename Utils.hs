module Utils (
              getFileContents,
              getFileHandles,
              intersperse_error,
              join,
              map_either,
              split_either
              ) where
import IO
import Data.List (intersperse)
-- import Either

getFileContents :: [String] -> [IO (Either (IOError, String) String)]
getFileContents [] = [getContents >>= return.Right]
getFileContents xs = map getOneFile xs

getFileHandles :: [String] -> [IO (Either (IOError, String) Handle)]
getFileHandles [] = [return (Right stdin)]
getFileHandles xs = map getOneHandle xs

getOneFile :: String -> IO (Either (IOError, String) String)
getOneFile "-" = getContents >>= return . Right
getOneFile  x  = withFile x (hGetContents)

getOneHandle :: String -> IO (Either (IOError, String) Handle)
getOneHandle "-" = return (Right stdin)
getOneHandle  x  = withFile x (return . id)

withFile :: FilePath -> (Handle -> IO a) -> IO (Either (IOError, String) a)
withFile f a = do rv <- try (do h <- openFile f ReadMode; a h)
                  return $ map_either (\x -> (x, f)) id rv

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

map_either :: (a -> c) -> (b -> d) -> Either a b -> Either c d
map_either f _ (Left x) = Left (f x)
map_either _ g (Right x) = Right (g x)
