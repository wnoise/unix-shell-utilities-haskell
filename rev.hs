-- bug?: if last line doesn't end with EOL, one will be added
-- but unix rev does this too.
-- does not take list of files
module Main(main) where
main = getContents >>= putStr . unlines . map reverse . lines
