import System.IO
import System.Random (randomRIO)

data Markov = Markov {
            key :: (String,String),
            value :: [String]
            }
type MarkovChain = [Markov]

instance Eq (Markov) where
    (Markov (a1,b1) _ ) == (Markov (a2,b2) _ ) =
      (a1 == a2) && (b1 == b2)

instance Show (Markov) where
    show a = (fst $ key a) ++ " " ++ (snd $ key a)

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

prntMarkov :: Markov -> IO String
prntMarkov x = do
    m <- pick $ value x
    return ((show x) ++ " " ++ m)

chain :: Markov -> Markov -> Markov
chain x y = Markov (key x) (value x ++ value y)

genMarkov :: [String] -> MarkovChain
genMarkov (x:xs) = Markov
  (x, xs !! 0)
  [xs !! 1] : (if length xs > 2
              then genMarkov xs
              else [])

grpMarkov :: MarkovChain -> MarkovChain
grpMarkov [] = []
grpMarkov (x:xs)
    | x `elem` xs = grpMarkov (map (\y -> if y == x then chain x y else y) xs)
    | otherwise   = x:grpMarkov xs

--loop markovchain
--print key and value
--recurse second key and value in list; decrement count
--repeat until count is 0

--create new key with second and rand value into recurse call
doMarkov :: MarkovChain -> Markov -> Integer -> IO String
doMarkov (x:xs) m count = prntMarkov x

main :: IO ()
main = do
    contents <- getLine
    let m = grpMarkov . genMarkov $ words contents
    let first = head $ tail $ m
    (doMarkov m first 0) >>= putStrLn :: IO ()
