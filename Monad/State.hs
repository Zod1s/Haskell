{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
newtype State s a = State {runState :: s -> (s, a)}

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put state = State (const (state, ()))

modify :: (s -> s) -> State s ()
modify f = get >>= (put . f) -- State (\s -> (f s, ()))

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State stateFn) = State (\s -> let (s', result) = stateFn s in (s', f result))

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure a = State (\s -> (s, a))

  -- (<*>) :: (<*>) :: State s (a -> b) -> State s a -> State s b
  (State stateFa) <*> (State stateA) = State (\s -> let (s', fa) = stateFa s; (s'', a) = stateA s' in (s'', fa a))

instance Monad (State s) where
  -- return :: a -> State s a
  return = pure

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  (State sa) >>= fn = State (\s -> let (s', a) = sa s in runState (fn a) s')

reverseWithCount :: [a] -> State Int [a]
reverseWithCount list = do
  modify (+ 1)
  pure (reverse list)

appendReversedWithCount :: [a] -> [a] -> State Int [a]
appendReversedWithCount list1 list2 = do
  revList1 <- reverseWithCount list1
  revList2 <- reverseWithCount list2
  modify (+ 1)
  pure (revList1 ++ revList2)

append3ReversedWithCount :: [a] -> [a] -> [a] -> State Int [a]
append3ReversedWithCount list1 list2 list3 = do
  revList1 <- reverseWithCount list1
  revList2 <- reverseWithCount list2
  revList3 <- reverseWithCount list3
  modify (+ 1)
  pure (revList1 ++ revList2 ++ revList3)