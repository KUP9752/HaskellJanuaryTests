import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

-------------- PART I 
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp item dict
  = head [y | (x, y) <- dict, x == item]

checkSat :: BDD -> Env -> Bool
checkSat (start, tree) env
  = checkSatHelper start
  where
      checkSatHelper 0  = False
      checkSatHelper 1  = True
      checkSatHelper i 
        | val           = checkSatHelper tBranch
        | otherwise     = checkSatHelper fBranch
        where
          (var, fBranch, tBranch) = lookUp i tree
          val                     = lookUp var env  

sat :: BDD -> [[(Index, Bool)]]
sat (start, tree)
  = satHelper start []
  where
    satHelper :: NodeId -> [(Index, Bool)] -> [[(Index, Bool)]]
    satHelper 0 _    
      = []
    satHelper 1 env 
      = [sort env]
    satHelper id env
      = satHelper fBranch ((i, False) : env) ++ (satHelper tBranch ((i, True) : env))
      where
        (i, fBranch, tBranch) = lookUp id tree

-------------- PART II
simplify :: BExp -> BExp
simplify (Not (Prim bool))
  = Prim (not bool)
simplify (And (Prim b) (Prim b'))
  = Prim (b && b')
simplify (Or (Prim b) (Prim b'))
  = Prim (b || b')
simplify exp
  = exp

restrict :: BExp -> Index -> Bool -> BExp
restrict bExp id bool
  = restrict' bExp
  where
    restrict' :: BExp ->  BExp
    restrict' (IdRef i)
      | i == id   = Prim bool
      | otherwise = IdRef i
    restrict' (Not exp)  
      = simplify (Not (restrict' exp))
    restrict' (And exp exp')
      = simplify (And (restrict' exp) (restrict' exp'))
    restrict' (Or exp exp')
      = simplify (Or (restrict' exp) (restrict' exp'))
    restrict' exp
      = simplify exp

---------------PART III 
-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD exp is = buildBDD' exp 2 is

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim False) id is
  = (0, [])
buildBDD' (Prim True) id is  
  = (1, [])
buildBDD' exp id (i : is)
  = (id, (id, (i, li, ri)) : (leftNodes ++ rightNodes))
  where
    (ri, rightNodes) = buildBDD' rexp (id * 2 + 1) is
    (li, leftNodes)  = buildBDD' lexp (id * 2) is
    rexp             = restrict exp i True
    lexp             = restrict exp i False


-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])