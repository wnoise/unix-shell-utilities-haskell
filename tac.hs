-- bug: unix tac concatenates last line with second-to last if file
-- doesn't end with newline.  We add a newline, instead.
-- arguments & file reading not implemented.
module Main(main) where
main = getContents >>= putStr . unlines . reverse . lines
