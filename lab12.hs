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

partitionLess :: Ord a => a -> [a] -> [a]
partitionLess v (t:ts) = filter (\x -> v >= x) t : ts

partitionMore :: Ord a => a -> [a] -> [a]
partitionMore v (t:ts) = filter (\x -> v < x) ts

-- Step 11 above, 12 below

-- longBoi :: [Num]
-- longBoi = [1,2,3,4]
--
-- longTwo :: [Num]
-- longTwo = [5,6,7]
--
-- longest :: [Num]
-- longest = longBoi : 5

quicksort :: Ord a => [a] -> [a]
quicksort (n:ns)
  | length ns == 1 = ns
  | otherwise = quicksort (partitionLess n ns) ++ quicksort (partitionMore n ns)
-- infinite type sadness, but this is the idea

-- quicksort (x:xs) = (quicksort (partitionLess x xs) ++ quicksort (partitionMore x xs))
-- more infinite type sadness?

-- pairToList :: Pair a -> [a]
-- pairToList a = [fsts a, snds a]
