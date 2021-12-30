module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes 
  = length . filter ('1' ==) . (flip showBitVector 16)

countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
  = countOnes (n .&. (bit i - 1))
    --length . filter ('1' ==) . (take i) . reverse . (flip showBitVector 16)

getIndex :: Int -> Int -> Int -> Int
getIndex n bIndex bSize
  = n' .&. (bit bSize - 1)
  where
    n'   = shiftR n (bIndex * bSize)
    

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace i xs val
  = (take i xs) ++ val : (drop (i + 1) xs)


recReplace :: Int -> [a] -> a -> [a]
recReplace _ [] _
  = []
recReplace 0 (x : xs) val
  = val : xs
recReplace i (x : xs) val
  = x : recReplace (i - 1) xs val

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt i val xs 
  = (take i xs) ++ val : (drop i xs)

recInstertAt :: Int -> a -> [a] -> [a]
recInstertAt _ val []
  = [val]
recInstertAt 0 val xs
  = val : xs
recInstertAt i val (x : xs)
  = x : recInstertAt (i - 1) val xs
--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie f1 f2 (Leaf ns) 
  = f2 ns
sumTrie f1 f2 (Node _ ss)
  = foldl (\n node -> sumTrie' node + n ) 0 ss
  where
    sumTrie' (Term int)
      = f1 int
    sumTrie' (SubTrie trie)
      = sumTrie f1 f2 trie 

--
-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.
--
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)


member :: Int -> Hash -> Trie -> Int -> Bool
member val nHash t bSize 
  =  memberHelper 0 t 
  where
    memberHelper :: Int -> Trie -> Bool
    memberHelper _ (Leaf ns)
      = elem val ns
    memberHelper layer (Node bVec ns) 
      | testBit bVec i  = subNodeMember layer (ns !! n)
      | otherwise       = False
      where
        i   = getIndex nHash layer bSize
        n   = countOnesFrom i bVec
        subNodeMember :: Int -> SubNode -> Bool
        subNodeMember layer (Term int)
          = int == val
        subNodeMember layer (SubTrie trie)
          = memberHelper (layer + 1) trie





--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hf maxD bSize val trie
  = insert' 0 val trie
  where
    insert' :: Int -> Int -> Trie -> Trie
    insert' layer val (Leaf ns)
      | elem val ns = Leaf ns
      | otherwise   = Leaf (val : ns)

    insert' layer val (Node bVec ns) 
      | layer == (maxD - 1) = Leaf [val]
      | testBit bVec i      = Node bVec (replace n ns ns')
      | otherwise           = Node (setBit bVec i) (insertAt n (Term val) ns)
      where
        i   = getIndex (hf val) layer bSize
        n   = countOnesFrom i bVec
        ns' = newNs (ns !! n)

        newNs (SubTrie trie')
          = SubTrie (insert' (layer + 1) val trie')
        newNs (Term int)
          | val == int = Term int
          | otherwise  = SubTrie (insert' (layer + 1) val trie')
          where
            trie' = insert' (layer + 1) int empty


buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie hf maxD bSize 
  = foldl (\t v -> insert hf maxD bSize v t ) empty  
