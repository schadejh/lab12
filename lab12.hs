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
  | length ns == 1 = ns
  | length ns > 1 = quicksort (partitionLess (head ns) (tail ns)) ++ [(head ns)] ++ quicksort (partitionMore (head ns) (tail ns))
    where partitionMore v ts = filter (\x -> x > v) ts
		  partitionLess v ts = filter (\x -> x <= v) ts

  | otherwise = []

-- pairToList :: Pair a -> [a]
-- pairToList a = [fsts a, snds a]
