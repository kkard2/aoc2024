import System.IO
import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "input"
    let inputLines = lines input
        col0 = map (\x -> read (words x !! 0) :: Integer) inputLines
        col1 = map (\x -> read (words x !! 1) :: Integer) inputLines
        col0Counts = map (\x -> foldl (\a b -> if b == x then (1 + a) else a) 0 col1) col0

    print (foldl (+) 0 (zipWith (*) col0 col0Counts))
