-- | Problem 46

and' :: Bool -> Bool -> Bool
and' True a = a
and' False _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _ = False

xor' :: Bool -> Bool -> Bool
xor' a b = not' a == b

not' :: Bool -> Bool
not' True = False
not' False = True