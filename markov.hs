import System.IO

data Markov = Markov {
            key :: (String,String),
            value :: [String]
            }
            deriving Show
type MarkovChain = [Markov]

instance Eq (Markov) where
    (Markov (a1,b1) _ ) == (Markov (a2,b2) _ ) =
      (a1 == a2) && (b1 == b2)

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
--take second key and value and find in list

main :: IO ()
main = do
    contents <- getLine
    print . grpMarkov . genMarkov $ words contents
