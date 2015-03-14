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

prntMarkov :: Markov -> String -> String
prntMarkov x s = s ++ " "

doMarkov :: MarkovChain -> Markov -> Integer -> IO ()
doMarkov (x:xs) m count = do
    n <- pick $ value m
    let t = Markov ((snd $ key m),(n)) []
    let tt = filter (\y -> y == t) (x:xs)
    putStr $ prntMarkov m n
    if not (null tt)
      then doMarkov (x:xs) (head tt) (count-1)
      else return ()

main :: IO ()
main = do
    contents <- getLine
    let m = grpMarkov . genMarkov $ words contents
    first <- pick m
    doMarkov m first 10
