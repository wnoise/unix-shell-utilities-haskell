-- bug?: if last line doesn't end with EOL, one will be added
-- but unix rev does this too.
-- does not take list of files
module Main(main) where
import IO
import System
import System.IO.Error
import Utils
main = do args <- getArgs
          conts <- sequence $ getFileContents args 
          intersperse_error error_handler file_handler conts

error_handler :: (IOError, FilePath) -> IO ()
error_handler (e, f) = do name <- getProgName
                          let error_string = ioeGetErrorString e
                          hPutStrLn stderr $ name ++ ": " ++ f ++ ": " ++ error_string

file_handler = putStr . unlines . map reverse . lines
