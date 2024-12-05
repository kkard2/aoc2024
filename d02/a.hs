import System.IO
import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "input"
    let inputLines = lines input
        list = map (\line -> map read (words line)) inputLines :: [[Integer]]

    print (length (filter isValid list))

isValid :: [Integer] -> Bool
isValid list = do
    let a = head list
    let b = tail list
    isValidImpl (<) a b || isValidImpl (>) a b

isValidImpl :: (Integer -> Integer -> Bool) -> Integer -> [Integer] -> Bool
isValidImpl pred prev [] = True
isValidImpl pred prev list = do
    let next = head list
    if pred prev next && (abs (prev - next)) <= 3 then
        isValidImpl pred next (tail list)
        else
            False
