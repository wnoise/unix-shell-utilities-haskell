module Utils (getFileContents, getFileHandles, join) where
import IO

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

|-- Perl's join operator, more or less.
join = (concat .) . intersperse
