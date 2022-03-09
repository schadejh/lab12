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
partitionLess v (t:ts) = filter (\x -> v >= x) ts

partitionMore :: Ord a => a -> [a] -> [a]
partitionMore v (t:ts) = filter (\x -> v < x) ts

-- Step 11 above, 12 below

quicksort :: [a] -> [a]
quicksort ns = quicksort (partitionLess (head ns) ns) : quicksort (partitionMore (head ns) ns)
-- infinite type sadness, but this is the idea

quicksort xs = quicksort lower : quicksort higher
  where lower = partitionLess (head xs) xs
       higher = partitionMore (head xs) xs
-- more infinite type sadness?

pairToList :: Pair a -> [a]
pairToList a = [fsts a, snds a]
