-- James Schader
-- 20220308

listOf4 :: a -> a -> a -> a -> [a]
listOf4 a b c d = [a,b,c,d]

listOf4With8 = listOf4 8

listOf4With88 = listOf4 8 8

double = (2*)

triple = (3*)

applyToList :: (a -> a) -> [a] -> [a]
applyToList _ [] = []
applyToList f (n:ns) = f n : applyToList f ns

-- partitionLess :: Ord a => a -> [a] -> [a]
-- partitionLess v ts = filter (\x -> x <= v) ts

-- partitionMore :: Ord a => a -> [a] -> [a]
-- partitionMore v ts = filter (\x -> x > v) ts

quicksort :: Ord a => [a] -> [a]
quicksort ns
  | length ns < 1 = []
  | length ns == 1 = ns
  | length ns > 1 = quicksort lower ++ [(head ns)] ++ quicksort upper
      where upper = filter (\x -> x > (head ns)) (tail ns)
            lower = filter (\x -> x <= (head ns)) (tail ns)
-- I don't need an otherwise clause, but how would I add that?

type Pair a = (a,a)

pairToList :: Pair a -> [a]
pairToList (p,q) = [p,q]

-- no safety for dim mismatch
pairUp :: Pair [a] -> [Pair a]
pairUp (ps,qs)
--  | length ps < 1 = ()
  | length ps ==1 = [(head ps, head qs)]
  | length ps > 1 = [(head ps, head qs)] ++ pairUp (tail ps, tail qs)

data Movement
  = North Int
  | South Int
  | East Int
  | West Int
  deriving (Show)

