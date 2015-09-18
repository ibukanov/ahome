module Symbols_Term(
  Symbol, Symbols_Term, 
  sterm, sterm0, sterm1, sterm2,
  is_same_symbols,
  mul_sterms,
  add_num_sterm,
  mul_num_sterm,
  neg_sterm
) where

import List(sort)

type Symbol = String
type NumCoef = Integer

data Symbols_Term = ST NumCoef [Symbol]

{-
sterm is the ONLY way to construct Symbols_Term as they insure internal consistancy
-}

sterm :: NumCoef -> [Symbol] -> Symbols_Term
sterm0 :: NumCoef -> Symbols_Term
sterm1 :: NumCoef -> Symbol -> Symbols_Term
sterm2 :: NumCoef -> Symbol -> Symbol -> Symbols_Term

sterm 0 list = ST 0 []
sterm n list = ST n (sort list)

sterm0 n = ST n []

sterm1 0 _ = ST 0 []
sterm1 n symbol = ST n [symbol]

sterm2 0 _ _ = ST 0 []
sterm2 n symbol_1 symbol_2 = ST n (sort (symbol_1 : [symbol_2]))

is_same_symbols :: Symbols_Term -> Symbols_Term -> Bool
is_same_symbols (ST _ l1) (ST _ l2) = l1 == l2

mul_sterms :: Symbols_Term -> Symbols_Term -> Symbols_Term
mul_sterms (ST n1 l1) (ST n2 l2) = sterm (n1 * n2) (l1 ++ l2)

add_num_sterm :: NumCoef -> Symbols_Term -> Symbols_Term
mul_num_sterm :: NumCoef -> Symbols_Term -> Symbols_Term
neg_sterm :: Symbols_Term -> Symbols_Term

-- optimization: use constructor directly
add_num_sterm n1 (ST n2 l) = if sum == 0 then ST 0 [] else ST sum l where
  sum = n1 + n2
  
mul_num_sterm n1 (ST n2 l) = if prod == 0 then ST 0 [] else ST prod l where
  prod = n1 * n2

neg_sterm (ST n l) = ST (-n) l

  

instance Eq Symbols_Term where
  (==) (ST n1 list1) (ST n2 list2) = n1 == n2 && list1 == list2

instance Ord Symbols_Term where
  compare (ST n1 list1) (ST n2 list2) = 
    case (compare list1 list2) of
      LT -> LT
      GT -> GT
      EQ -> compare n1 n2

instance Show Symbols_Term where
  showsPrec _ (ST n list) s = do_shows n list s where
    do_shows 0 _ s = '0' : s
    do_shows 1 (x:xs) s = shows x (show_symbols xs s)
    do_shows n list s = shows n (show_symbols list s)
    show_symbols [] s = s 
    show_symbols (x:xs) s = '*' : shows x (show_symbols xs s)



