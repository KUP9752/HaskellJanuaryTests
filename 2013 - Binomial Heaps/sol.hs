import Data.List (delete)

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node a _ _)
  = a

rank :: BinTree a -> Int
rank (Node _ r _)
  = r

children :: BinTree a -> [BinTree a]
children (Node _ _ c)
  = c

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t @ (Node v r bts) t'@ (Node v' r' bts')
  | v < v'    = Node v  (r + 1) (t' : bts )
  | otherwise = Node v' (r + 1) (t  : bts')

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin bts
  = minimum( map value bts )

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] bh
  = bh
mergeHeaps bh []
  = bh
mergeHeaps h @ (t : bh) h' @ (t' : bh')
  | rank t  < rank t' = t  : mergeHeaps bh  h'
  | rank t' < rank t  = t' : mergeHeaps bh' h
  | otherwise         = mergeHeaps [combineTrees t t'] (mergeHeaps bh bh')
  
insert :: Ord a => a -> BinHeap a -> BinHeap a
insert v bh
  = mergeHeaps [Node v 0 []] bh

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin []
  = []
deleteMin bh
  = mergeHeaps (delete deleteTree bh) (reverse bh')
  where
    deleteTree @ (Node _ _ (bh')) = lowestValueTree (tail bh) (head bh)
  
    lowestValueTree :: Ord a => BinHeap a -> BinTree a -> BinTree a
    lowestValueTree [] t
      = t
    lowestValueTree (t' : bh) t
      | value t < value t' = lowestValueTree bh t
      | otherwise          = lowestValueTree bh t'

binSort :: Ord a => [a] -> [a]
binSort vs
  = binSort' bh
  where
    bh = foldl (flip $ insert) [] vs
    
    binSort' :: Ord a => BinHeap a -> [a]
    binSort' []
      = []
    binSort' bh'
      = extractMin bh' : (binSort' (deleteMin bh'))
--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary h
  = toBinary' h (-1)
  where
    toBinary' :: BinHeap a -> Int -> [Int]
    toBinary' [] _
      = []
    toBinary' (t : h) i
      = (toBinary' h r) ++ (1 : take (r - i - 1) [0,0..] )
      where
        r = rank t
  
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


