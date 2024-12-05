import System.IO
import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "input"
    let inputLines = lines input
        list = map (\line -> map read (words line)) inputLines :: [[Integer]]

    print (length (filter isValidDampened list))

isValidDampened :: [Integer] -> Bool
isValidDampened list = isValid list || any id (map isValid (walker [] (head list) (tail list)))

-- i have no idea why i named this walker
walker :: [Integer] -> Integer -> [Integer] -> [[Integer]]
walker h cur [] = [h]
walker h cur t = [h ++ t] ++ walker (h ++ [cur]) (head t) (tail t)

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
