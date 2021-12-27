import Data.List hiding (insert)

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)  --value, rank subtrees
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node val _ _) 
  = val

rank :: BinTree a -> Int
rank (Node _ rank _ ) 
  = rank


children :: BinTree a -> [BinTree a]
children (Node  _ _ cs)
  = cs

-- Only works if they are the same rank
combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees  t t'
  | rVal >= rVal' = Node rVal' (r' + 1) (t : cs')
  | otherwise     = Node rVal (r + 1) (t' : cs)
  where
    rVal  = value t
    rVal' = value t' 
    (r, r') = (rank t, rank t')
    cs    = children t
    cs'   = children t'

    
    
--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin = minimum . map value

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h []  = h
mergeHeaps [] h' = h'
mergeHeaps h@(t : ts) h'@(t' : ts') 
  | rank t < rank t'     = [t] ++ mergeHeaps ts h'
  | rank t' < rank t     = [t'] ++ mergeHeaps h ts'
  | otherwise            = mergeHeaps [combineTrees t t'] (mergeHeaps ts ts')


insert :: Ord a => a -> BinHeap a -> BinHeap a
insert val h
  = mergeHeaps [Node val 0 []] h

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h
  = mergeHeaps h' (remove (minTree h) h)
  where 
    minVal                 = extractMin h
    h'                     = (reverse . children $ minTree h)
    minTree (t : ts)
      | value t == minVal  = t
      | otherwise          = minTree ts

remove :: Eq a => a ->  [a] -> [a]
remove remT (t : ts)
  | t == remT  = ts
  | otherwise  = t : remove remT ts

makeHeap :: Ord a => [a] -> BinHeap a
makeHeap items
  = makeHelper items []
  where 
    makeHelper :: Ord a => [a] -> BinHeap a -> BinHeap a
    makeHelper [] h = h
    makeHelper (i : is) h = makeHelper is (insert i h)

binSort :: Ord a => [a] -> [a]
binSort items
  = sortHelper (makeHeap items)
  where 
    h = makeHeap items
    sortHelper :: Ord a => BinHeap a -> [a]
    sortHelper [] = []
    sortHelper h  = extractMin h : sortHelper (deleteMin h)

-------------------------- ONLINE SOLUTIONS
makeHeap' :: Ord a => [a] -> BinHeap a
makeHeap'
  = foldr (mergeHeaps . flip insert []) []

sortHeap' :: Ord a => BinHeap a -> [a]
sortHeap'
  = unfoldr maybeMin
  where
    maybeMin h'
      | null h'   = Nothing
      | otherwise = Just (extractMin h', deleteMin h')

binSort' :: Ord a => [a] -> [a]
binSort'
  = sortHeap' . makeHeap
--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary
  = undefined

binarySum :: [Int] -> [Int] -> [Int]
binarySum
  = undefined

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]



