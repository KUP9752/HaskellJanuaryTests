module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x list
  =  length $ filter (x ==) list

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (nodes, edges)
  = [(n, count n connections + count n connections') | n <- nodes ]
  where 
    connections = map fst edges
    connections'= map snd edges

neighbours :: Eq a => a -> Graph a -> [a]
neighbours node (nodes, edges)
  = [e | (e, e') <- edges, e' == node] ++ [e' | (e, e') <-edges, e == node]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode rnode (nodes, edges)
  = (filter (rnode /=) nodes, [edge | edge@(e, e') <- edges, e /= rnode && e' /= rnode])

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([],_) 
  = []
colourGraph numCol g 
  | null unusedC  = (n, 0) : cMap     --0 Stands for not colouring
  | otherwise     = (n, minimum unusedC) : cMap 
  where
    n             = (snd . minimum) [(j,i) | (i,j) <- degrees g]
    g'            = removeNode n g
    cMap          = colourGraph numCol g'
    usedC         = [lookUp neighbour cMap | neighbour <- neighbours n g]
    unusedC       = [1..numCol] \\ usedC


  ------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap 
  = undefined

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments 
  = undefined

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp 
  = undefined

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock 
  = undefined

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG 
  = undefined

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined