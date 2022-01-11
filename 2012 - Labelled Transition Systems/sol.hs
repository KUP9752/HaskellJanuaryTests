import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp x xs
  = fromJust (lookup x xs)

states :: LTS -> [State]
states lts
  = nub states
  where
    (s, _) = unzip lts
    states = concat (map (\(i1, i2) -> i1 : i2 : []) s)
    
transitions :: State -> LTS -> [Transition]
transitions f lts
  = [t | t @ ((from, _), _) <- lts, from == f]

alphabet :: LTS -> Alphabet
alphabet lts
  = nub ids
  where
     (_, ids) = unzip lts

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions (Prefix i p)
  = i : actions p
actions (Choice ps)
  = concat (map actions ps)
actions _
  = []

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts ids pds
  = accepts' ids (head pds)
  where
    accepts' :: [Id] -> ProcessDef -> Bool
    accepts' [] _
      = True
    accepts' _ (_, STOP)
      = False
    accepts' target (id, Ref i)
      = accepts' target (id, lookUp i pds)
    accepts' target (id, Prefix i p)
      | i /= head target = False
      | otherwise        = accepts' (tail target) (id, p)
    accepts' target (id, Choice ps)
      = or (map (\p -> accepts' target (id, p)) ps)

------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition 
                   -> Alphabet -> Alphabet 
                   -> StateMap 
                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), a) ((s', t'), a') a1 a2 m
  | a == a'                  = [((lookUp (s, s') m, lookUp (t, t') m), a )]
  | elem a  a2 && elem a' a1 = []
  | elem a' a1               = [((lookUp (s, s') m, lookUp (t, s') m), a )]
  | elem a  a2               = [((lookUp (s, s') m, lookUp (s, t') m), a')]
  | otherwise                = ((lookUp (s, s') m, lookUp (s, t') m), a') :
                               [((lookUp (s, s') m, lookUp (t, s') m), a )]

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = visit 0 []
  where
    visit :: State -> [State] -> [Transition]
    visit s ss 
      | elem s ss = []
      | otherwise = tran ++ next
        where
          tran   = transitions s ts 
          next   = concat (map (\s' -> visit s' (s : ss)) target)
          target = [to | ((_, to), _) <- tran]

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose lts lts'
  = filter (\((_, _), a) -> a /= "$" && a /= "$'") (nub (pruneTransitions trans))
  where
    cartesian = [(s, s') | s <- states lts, s' <- states lts']
    mapping = zip cartesian [0..]
    trans = concat [composeTransitions t1 t2 
            ("$" : alphabet lts) ("$'" : alphabet lts') mapping | 
            (s, s') <- cartesian, t1 <- (((s, s), "$") : transitions s lts), 
            t2 <- (((s', s'), "$'") : transitions s' lts')]

--the compose below is an one-liner part IV function :)

{-compose :: LTS -> LTS -> LTS
compose lts lts'
  = filter (\((_, _), a) -> a /= "$" && a /= "$'") 
  (nub (pruneTransitions (concat 
  [composeTransitions t1 t2 ("$" : alphabet lts) ("$'" : alphabet lts')
  (zip [(s, s') | s <- states lts, s' <- states lts'] [0..]) | 
    (s, s') <- [(s, s') | s <- states lts, s' <- states lts'], 
    t1 <- (((s, s), "$") : transitions s lts), 
    t2 <- (((s', s'), "$'") : transitions s' lts')])))-}

------------------------------------------------------
buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS processDefs -- [(id,Process)]
  = (nub ((\(x,_,_) -> x) (build 0 (length processes) (head processes) (zip processes [0..]) [])))
  where
    getRef id (Ref id2)
      = getRef id2 (lookUp id processDefs)
    getRef id _
      = (Ref id)
    normalise (Ref id)
      = getRef id (lookUp id processDefs)
    normalise (STOP)
      = STOP
    normalise (Prefix id nextp)
      = (Prefix id (normalise nextp))
    normalise (Choice nextps)
      = (Choice (map normalise nextps))
    newDefs []
      = []
    newDefs ((id, (Ref id2)) : t)
      = newDefs ((id2, lookUp id2 processDefs) : t)
    newDefs ((id, nextp) : t)
      = ((id, normalise nextp) : newDefs t)
    processDefs' = nub (newDefs processDefs)
    processes = map snd processDefs'
    build :: State -> State -> Process -> [(Process, State)] -> [Process] -> (LTS, State, [(Process, State)])
    build curent next process pds visited
      | elem process visited
        = ([], next, pds)
    build curent next STOP pds visited
      = ([], next, pds)
    build curent next (Prefix id (Ref id')) pds visited
      = (((curent, procState), id) : lts, nextState, nextpds)
      where
        nextp = lookUp id' processDefs'
        procState = lookUp nextp pds
        (lts, nextState, nextpds) = build procState next nextp pds ((Prefix id (Ref id')) : visited)
    build curent next (Prefix id nextp) pds visited
      | elem nextp visited || elem nextp (map fst pds)
        = (((curent, procState), id) : lts, nextState, nextpds)
      | otherwise
        = ((((curent, next), id) : lts'), nextState', nextpds')
        where
          procState = lookUp nextp pds
          (lts, nextState, nextpds) = build procState next nextp pds ((Prefix id nextp) : visited)
          (lts', nextState', nextpds') = build next (next+1) nextp ((nextp, next) : pds) ((Prefix id nextp) : visited)
    build curent next (Choice []) pds visited
      = ([], next, pds)
    build curent next (Choice (nextp : t)) pds visited
      = (lts ++ lts', nextState', nextpds')
      where
        (lts, nextState, nextpds) = build curent next nextp pds ((Choice (nextp : t)) : visited)
        (lts', nextState', nextpds') = build curent nextState (Choice t) nextpds ((Choice (nextp : t)) : visited)

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]
