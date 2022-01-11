module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

-- FULL MARKS ACHIEVED 31/30
--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes 0 
  = 0
countOnes n
  = bitTable !! (n .&. (bit 4 - 1)) 
    + countOnes (shiftR n 4)

-- gets number of 1's to right of (i) in bin number(n)
countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
  = countOnes (n .&. (bit i - 1))

-- gets an offset(i) blocks of bits(b) from bin number(n)
getIndex :: Int -> Int -> Int -> Int
getIndex n i b
  = (bit b - 1) .&. shiftR n (i*b)

replace :: Int -> [a] -> a -> [a]
-- Pre: the index is less than the length of the list
replace _ [] _ 
  = []
replace 0 (x:xs) v
  = v : xs 
replace i (x:xs) v
  = x : replace (i-1) xs v

insertAt :: Int -> a -> [a] -> [a]
-- Pre: the index is less than or equal to the length of the list
insertAt _ v []  
  = [v]
insertAt 0 v lst 
  = v : lst
insertAt i v (x:xs) 
  = x : insertAt (i-1) v xs 

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie f g (Leaf ns) 
  = g ns
sumTrie f g (Node _ snodes)  
  = foldl (\n node -> sumTrie' node + n) 0 snodes
  where
  sumTrie' (Term n)  
    = f n
  sumTrie' (SubTrie t) 
    = sumTrie f g t


trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)

-- naming convention v = val, h = hashed val, 
-- t = trie, b = blocksize
member :: Int -> Hash -> Trie -> Int -> Bool
member v h t b = member' 0 t
    where
    member' clvl (Leaf ns)
      = elem v ns
    member' clvl (Node inode snodes)
      | testBit inode i = memberOfSubnode clvl (snodes !! n)
      | otherwise       = False 
      where
        i = getIndex h clvl b
        n = countOnesFrom i inode 
    memberOfSubnode clvl (Term n) 
      = v == n
    memberOfSubnode clvl (SubTrie t)
      = member' (clvl + 1) t



--------------------------------------------------------------------
-- Part III
-- naming convention hf = hash func, maxD = maxDepth 
-- b = blocksize, v = current val, t = trie, 
insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hf maxD b val t = insert' 0 val t
  where
  insert' clvl v (Leaf ns)
    | elem v ns = Leaf ns
    | otherwise = Leaf (v : ns)

  insert' clvl v (Node inode snodes)
    | clvl == (maxD - 1) = Leaf [v]
    | testBit inode i 
      = Node inode (replace n snodes snode')
    | otherwise       
      = Node (setBit inode i) (insertAt n (Term v) snodes)
    where
      i = getIndex (hf v) clvl b
      n = countOnesFrom i inode 
      snode' = newSnode (snodes !! n)
      newSnode (SubTrie t')
        = SubTrie (insert' (clvl +1) v t')
      newSnode (Term v')
        | v == v' = Term v'
        | otherwise = SubTrie (insert' (clvl + 1) v t')  
        where
            t' = insert' (clvl + 1) v' empty

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie hf maxD b ns 
  = foldl (\t val -> insert hf maxD b val t) empty ns
