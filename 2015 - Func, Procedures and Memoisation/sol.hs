import Data.Maybe

-- All programs are assumed to be well-formed in the following sense:
--
-- All operators, functions and procedures will always be applied
-- to the correct number of arguments, all of which will be of the appropriate
-- type.
--
-- Boolean-valued expressions will always evaluate to either 0 (false) or 1
-- (true).
--
-- In an array element assignment the array being assigned to will always be 
-- in scope.
--
-- In a procedure call of the form Call x p es the procedure p will always exit 
-- via a Return statement.
--
-- A Return statement will always be the last statement to be executed in a 
-- procedure's defining code block (there is no `dead code').
--

--------------------------------------------------------------------
type Id = String

data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)

data Op = Add | Mul | Less | Equal | Index
          deriving (Eq, Show)

data Exp = Const Value | 
           Var Id | 
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp] 
         deriving (Eq, Show)

type FunDef = (Id, ([Id], Exp))

type Block = [Statement]

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp 
               deriving (Eq, Show)

type ProcDef = (Id, ([Id], Block))

data Scope = Local | Global
           deriving (Eq, Show)

type Binding = (Id, (Scope, Value))

type State = [Binding]

--------------------------------------------------------------------
-- Part I

getValue :: Id -> State -> Value
-- Pre: The identifier has a binding in the state
getValue i s
  = v
  where
    (_, v) = lookUp i s

getLocals :: State -> State
getLocals s
  = [b | b @ (_, (scope, _)) <- s, scope == Local]

getGlobals :: State -> State
getGlobals s
  = [b | b @ (_, (scope, _)) <- s, scope == Global]

assignArray :: Value -> Value -> Value -> Value
-- The arguments are the array, index and (new) value respectively
-- Pre: The three values have the appropriate value types (array (A), 
--      integer (I) and integer (I)) respectively.
assignArray (A array) (I i) (I v)
  = A ((i, v) : (filter (\(x, y) -> x /= i) array))

updateVar :: (Id, Value) -> State -> State
updateVar (x, v) []
  = [(x, (Local, v))]
updateVar (x, v) (original @ (id, (scope, value)) : ss)
  | id == x = (x, (scope, v)) : ss
  | otherwise = original : (updateVar (x, v) ss)

---------------------------------------------------------------------
-- Part II

applyOp :: Op -> Value -> Value -> Value
-- Pre: The values have the appropriate types (I or A) for each primitive
applyOp Add (I x) (I y) 
  = I (x + y)
applyOp Mul (I x) (I y) 
  = I (x * y)
applyOp Less (I x) (I y) 
  | x < y     = I 1
  | otherwise = I 0
applyOp Equal (I x) (I y) 
  | x == y    = I 1
  | otherwise = I 0
applyOp Index (A a) (I i) 
  = case lookup i a of
    Nothing -> I 0
    Just x  -> I x
  
bindArgs :: [Id] -> [Value] -> State
-- Pre: the lists have the same length
bindArgs id val
  = zipWith (\i v -> (i, (Local, v))) id val

evalArgs :: [Exp] -> [FunDef] -> State -> [Value]
evalArgs es def state
  = [eval e def state | e <- es]

eval :: Exp -> [FunDef] -> State -> Value
-- Pre: All expressions are well formed
-- Pre: All variables referenced have bindings in the given state
eval (Const c) def state
  = c
eval (Var id) def state
  = getValue id state
eval (Cond e1 e2 e3) def state
  | eval e1 def state == I 1 = eval e2 def state
  | otherwise                = eval e3 def state
eval (OpApp o e1 e2) def state
  = applyOp o (eval e1 def state) (eval e2 def state)
eval (FunApp f es) def state
  = eval e def (bound ++ state)
  where
    (as, e) = lookUp f def
    vs      = evalArgs es def state
    bound   = bindArgs as vs
  
---------------------------------------------------------------------
-- Part III

executeStatement :: Statement -> [FunDef] -> [ProcDef] -> State -> State
-- Pre: All statements are well formed 
-- Pre: For array element assignment (AssignA) the array variable is in scope,
--      i.e. it has a binding in the given state
executeStatement (Assign id exp) funs _ state
  = updateVar (id, eval exp funs state) state
executeStatement (AssignA id pos exp) funs procs state
  = updateVar (id, (assignArray array (eval pos funs state) (eval exp funs state))) state
  where
    array = getValue id state
executeStatement (If exp bl1 bl2) funs procs state
  | eval exp funs state == (I 1)
    = executeBlock bl1 funs procs state
  | otherwise                
    = executeBlock bl2 funs procs state
executeStatement (While exp bl) funs procs state
  | eval exp funs state == (I 1)
    = executeStatement (While exp bl) funs procs newState
  | otherwise = state
  where
    newState = executeBlock bl funs procs state
executeStatement (Call id1 id2 exps) funs procs state
  | id1 /= ""
    = updateVar (id1, getValue "$res" newState) returnS
  | otherwise
    = returnS
  where
    globals = getGlobals state
    (vars, bl) = lookUp id2 procs
    vals = evalArgs exps funs state
    localState = bindArgs vars vals
    newState = executeBlock bl funs procs (localState ++ globals)
    returnS = (getLocals state) ++ (getGlobals newState)
executeStatement (Return exp) funs procs state
  = updateVar ("$res", eval exp funs state) (getGlobals state)

executeBlock :: Block -> [FunDef] -> [ProcDef] -> State -> State
-- Pre: All code blocks and associated statements are well formed
executeBlock [] _ _ state
  = state
executeBlock (bh : bt) funs procs state
  = executeBlock bt funs procs newState
  where
    newState = executeStatement bh funs procs state

---------------------------------------------------------------------
-- Part IV

translate :: FunDef -> Id -> [(Id, Id)] -> ProcDef
translate (name, (as, e)) newName nameMap 
  = (newName, (as, b ++ [Return e']))
  where
    (b, e', ids') = translate' e nameMap ['$' : show n | n <- [1..]] 

translate' :: Exp -> [(Id, Id)] -> [Id] -> (Block, Exp, [Id])
translate' (Const val) maps vars
  = ([Assign (head vars) (Const val)], Const val, tail vars)
translate' (Var id) maps vars
  = ([Assign (head vars) (Var id)], (Var id), tail vars) 
translate' (OpApp op exp1 exp2) maps vars
  = (b1 ++ b2 ++ [(Assign (head vars2) (OpApp op (Var (peek b1)) (Var (peek b2))))], 
     (Var (head vars2)), tail vars2)
  where
    (b1, newExp1, vars1) = translate' exp1 maps vars
    (b2, newExp2, vars2) = translate' exp2 maps vars1
translate' (Cond exp exp1 exp2) maps vars
  =([If exp (b1 ++ [(Assign (head vars2) (Var (peek b1)))]) (b2 ++ [(Assign (head vars2) (Var (peek b2)))])], 
     (Var (head vars2)), tail vars2)
  where
    (b1, newExp1, vars1) = translate' exp1 maps vars
    (b2, newExp2, vars2) = translate' exp2 maps vars1
translate' (FunApp id exp) maps vars
  = ([Call (head vars) (lookUp id maps) exp], FunApp (lookUp id maps) exp, tail vars)

-- Finds the last assign-like operation in a block
-- Pre: All block end with an assignment of the types given below
peek :: Block -> Id
peek (_: h: t) = peek (h : t)
peek ((Assign id _) : []) = id
peek ((If _ exp1 exp2) : []) = peek exp1
peek ((Call id _ _) : []) = id

---------------------------------------------------------------------
-- PREDEFINED FUNCTIONS

-- A helpful predefined lookUp function...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t
  = fromMaybe (error ("\nAttempt to lookUp " ++ show x ++ 
                      " in a table that only has the bindings: " ++ 
                      show (map fst t))) 
              (lookup x t)

 -- Turns an int into an Exp...
intToExp :: Int -> Exp
intToExp n
  = Const (I n)

-- Turns a list of ints into an array Exp...
listToExp :: [Int] -> Exp
listToExp 
  = Const . listToVal

-- Turns a list of ints into an array Value...
listToVal :: [Int] -> Value
listToVal xs
  = A (zip [0..] xs)

-- memoise generates a procedure that caches values computed by function f.  
-- In general f will be a variant of some originally recursive function 
-- that calls the procedure generated here (named p) instead of itself.
-- Arguments:
--    p = procedure name; a = argument name; f = function variant; 
--    pt = 'isPresent' table; mt = memo table.

memoise :: Id -> Id -> Id -> Id -> Id -> ProcDef
memoise p a f pt mt
  = (p, 
     ([a], [If (OpApp Equal (OpApp Index (Var pt) (Var a)) (Const (I 0)))
               [Call "x" f [Var a],
                AssignA pt (Var a) (Const (I 1)),
                AssignA mt (Var a) (Var "x")
               ]
               [],
            Return (OpApp Index (Var mt) (Var a))
           ]
     )
    )


---------------------------------------------------------------------
-- Predefined States, arrays and expressions for testing...

sampleState, gState, fibState :: State
sampleState 
  = [("x", (Local, I 5)), ("y", (Global, I 2)), ("a", (Global, listToVal [4,2,7]))]

gState 
  = [("gSum", (Global, I 0))]

fibState 
  = [("fibPres", (Global, A [])), ("fibTab", (Global, A []))]

sampleArray :: Exp
sampleArray 
  = Const (listToVal [9,5,7,1])

e1, e2, e3, e4, e5 :: Exp
e1 = Const (I 1)
e2 = Var "y"
e3 = OpApp Add (Var "x") (Const (I 2))
e4 = Cond e1 (Var "x") (Const (I 9))
e5 = FunApp "fib" [Const (I 6)]

---------------------------------------------------------------------
-- Example (pure) functions for testing...

-- Equivalent of Haskell's max function...
biggest :: FunDef
biggest
  = ("biggest",
     (["m", "n"], Cond (OpApp Less (Var "m") (Var "n"))
                       (Var "n")
                       (Var "m"))
    )

-- Factorial, equivalent to: if n == 0 then 1 else n * fact (n - 1)...
fac :: FunDef
fac
  = ("fac",
     (["n"], Cond (OpApp Equal (Var "n") (intToExp 0))
                  (intToExp 1)
                  (OpApp Mul (Var "n") 
                             (FunApp "fac" [OpApp Add (Var "n") (intToExp (-1))])))
    )

-- Sums elements 0..n of an array...
sumA :: FunDef
sumA
  = ("sumA",
     (["a", "n"], Cond (OpApp Less (Var "n") (Const (I 0)))
                       (Const (I 0))
                       (OpApp Add (OpApp Index (Var "a") (Var "n"))
                                  (FunApp "sumA"
                                     [Var "a", OpApp Add (Var "n")
                                                         (Const (I (-1)))]))
     )
    )


-- Vanilla Haskell fib
fibH :: Int -> Int
-- Pre: n > 0
fibH n 
  = if n < 3 then 1 else fibH (n-1) + fibH (n-2)

-- fib in the purely functional subset
fib :: FunDef
fib
  = ("fib",
     (["n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Add (Var "n") (Const (I (-1)))])
                             (FunApp "fib" [OpApp Add (Var "n") (Const (I (-2)))]))
     )
    )

-- May be useful for testing translate...?
testFun :: FunDef
testFun
  = ("testFun",
     (["x", "y"], Cond (OpApp Equal (Var "x") (Var "y"))
                       (Cond (FunApp "p" [Var "y"])
                             (OpApp Add (Var "x") (Const (I 1)))
                             (OpApp Add (Var "x") (Var "y")))
                       (OpApp Add (FunApp "g" [Var "y"]) (Const (I 2))))
    )

---------------------------------------------------------------------
-- Example procedures for testing...

-- Add two integers and assign the result to a global variable, gSum, 
-- that is assumed to be in scope when the procedure is called...
gAdd :: ProcDef
gAdd
  = ("gAdd", 
     (["x", "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Sums elements 0..n of an array...
sumA' :: ProcDef
sumA'
  = ("sumA'",
     (["a", "n"], [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s") 
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- A procedural version of fib...
fibP :: ProcDef
-- Pre: n > 0
fibP 
  = ("fibP", 
     (["n"], [If (OpApp Less (Var "n") (Const (I 3))) 
                 [Return (Const (I 1))]
                 [Call "f1" "fibP" [OpApp Add (Var "n") (Const (I (-1)))], 
                  Call "f2" "fibP" [OpApp Add (Var "n") (Const (I (-2)))], 
                  Return (OpApp Add (Var "f1") (Var "f2"))
                 ]
             ]
     )
    )

fibManager :: ProcDef
fibManager
  = ("fibManager",
     (["n"], [If (OpApp Equal (OpApp Index (Var "fibPres") (Var "n")) 
                              (Const (I 0))) 
                 [Call "x" "fibM" [Var "n"], 
                  AssignA "fibPres" (Var "n") (Const (I 1)), 
                  AssignA "fibTab" (Var "n") (Var "x")
                 ] 
                 [], 
              Return (OpApp Index (Var "fibTab") (Var "n"))
             ]
     )
    )

fibM :: ProcDef
-- Pre: n > 0
-- The value of fibMGenerator below
fibM 
  = ("fibM", 
     (["n"], [If (OpApp Less (Var "n") (Const (I 3)))
                 [Assign "$3" (Const (I 1))]
                 [Call "$1" "fibManager" [OpApp Add (Var "n") (Const (I (-1)))],
                  Call "$2" "fibManager" [OpApp Add (Var "n") (Const (I (-2)))],
                  Assign "$3" (OpApp Add (Var "$1") (Var "$2"))
                 ],
              Return (Var "$3")
             ]
     )
    )

-- NewFib generated by translate
newFib :: ProcDef
newFib 
  = ("newFib",
     (["n"], [If (OpApp Less (Var "n") (Const (I 3))) 
                 [Assign "$1" (Const (I 1)), 
                  Assign "$5" (Var "$1")] 
                 [Call "$2" "fibManager" [OpApp Add (Var "n") (Const (I (-1)))], 
                  Call "$3" "fibManager" [OpApp Add (Var "n") (Const (I (-2)))], 
                  Assign "$4" (OpApp Add (Var "$2") (Var "$3")), 
                  Assign "$5" (Var "$4")
                 ], 
              Return (Var "$5")
             ]
     )
    )

---------------------------------------------------------------------
-- Sample top-level calls for testing...

-- This instantiates the table manager template (predefined)...
fibTableManager :: ProcDef
fibTableManager 
  = memoise "fibManager" "n" "fibM" "fibPres" "fibTab"

-- This uses the translate function to build the procedural, memoised,
-- version of fib...
fibMGenerator :: ProcDef
fibMGenerator 
  = translate fib "fibM" [("fib", "fibManager")] 
                   

-- Useful predefined executors...

execBiggest :: Int -> Int -> State
execBiggest m n
  = executeBlock [Return (FunApp "biggest" [intToExp m, intToExp n])] [biggest] [] []

execFac :: Int -> State
execFac n
  = executeBlock [Return (FunApp "fac" [intToExp n])] [fac] [] []

execSumA :: [Int] -> Int -> State
execSumA a n
  = executeBlock [Return (FunApp "sumA" [listToExp a, intToExp n])] [sumA] [] []

execGAdd :: Int -> Int -> State
execGAdd x y
  = executeBlock [Call "" "gAdd" [intToExp x, intToExp y]] [] [gAdd] gState

execSumA' :: [Int] -> Int -> State
execSumA' a n
  = executeBlock [Call "s" "sumA'" [listToExp a, intToExp n]] [] [sumA'] []

execGlobalSumA' :: [Int] -> Int -> State
execGlobalSumA' a n
  = executeBlock [Call "s" "sumA'" [listToExp a, intToExp n]] 
                 [] [sumA'] [("s", (Global, I 0))]

execFibP :: Int -> State
execFibP n 
  = executeBlock [Call "f" "fibP" [intToExp n]] [] [fibP] fibState

execFibM :: Int -> State
execFibM n 
  = executeBlock [Call "f" "fibM" [intToExp n]] [] [fibM, fibManager] fibState
