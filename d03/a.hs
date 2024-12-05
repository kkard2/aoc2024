import System.IO
import Data.List
import Text.Read

-- this code is terrible

main :: IO ()
main = do
    input <- readFile "input"
    print (solve 0 input)

solve :: Integer -> [Char] -> Integer
solve acc [] = acc
solve acc str = do
    if isPrefixOf "mul(" str then do
        let args = getBetween '(' ')' str
        let arg0 = maybe Nothing (\args -> getUntil ',' args) args
        let arg1 = maybe Nothing (\args -> getBetween ',' ')' args) args
        let arg0int = maybe Nothing (\arg0 -> readMaybe arg0) arg0 :: Maybe Integer
        let arg1int = maybe Nothing (\arg1 -> readMaybe arg1) arg1 :: Maybe Integer

        let result = case (arg0int, arg1int) of
                (Just a0, Just a1) -> a0 * a1
                _ -> 0

        solve (acc + result) (tail str)

        else
            solve acc (tail str)

getUntil :: Eq a => a -> [a] -> Maybe [a]
getUntil el [] = Nothing
getUntil el list = do
    let h = head list
    if h == el then Just [] else case getUntil el (tail list) of
        Just rest -> Just ([h] ++ rest)
        Nothing -> Nothing

getBetween :: Eq a => a -> a -> [a] -> Maybe [a]
getBetween l r [] = Nothing
getBetween l r list = do
    let h = head list
    if h == l then getUntil r (tail list)
        else getBetween l r (tail list)

skipUntil :: Eq a => a -> [a] -> Maybe [a]
skipUntil el [] = Nothing
skipUntil el list = do
    let h = head list
    let t = tail list
    if h == el then Just t else skipUntil el t
