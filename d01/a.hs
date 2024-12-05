import System.IO
import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "input"
    let inputLines = lines input
        col0 = map (\x -> read (words x !! 0) :: Integer) inputLines
        col1 = map (\x -> read (words x !! 1) :: Integer) inputLines
        sortedCol0 = sort col0
        sortedCol1 = sort col1

    print (foldl (+) 0 (map abs (zipWith (-) sortedCol0 sortedCol1)))
