module Poly (
  Poly, mk_poly, list_to_poly,
  map_poly,
  shows_poly
) where

import List(sort, sortBy)

import Misc_Util

default ()

type Power = Integer

data Num a => Poly a = Poly [(Power, a)] deriving Eq

mk_poly :: Num a => [(Power, a)] -> Poly a
mk_poly [] = Poly []
mk_poly list
  = (Poly . remove_0 . fuse_groups_by fuser . sortBy sorter) list
  where
  fuser (n1, x1) (n2, x2) = if n1 == n2 then Just (n1, x1 + x2) else Nothing
  -- Due to nature of tuple sorting sort == sortBy sorter but I do not want
  -- to depend on it
  sorter (n1, _) (n2, _) = compare n1 n2

list_to_poly :: Num a => [a] -> Poly a
list_to_poly = Poly . mk_pair_list 0
  where
  mk_pair_list _ [] = []
  mk_pair_list i (x:xs) = if x == 0 then r else (i, x) : r
    where r = mk_pair_list (i + 1) xs
    
map_poly :: (Num a, Num b) => (a -> b) -> Poly a -> Poly b
map_poly f = Poly . remove_0 . map (\(n, x) -> (n, f x)) . get_list

remove_0 :: Num a => [(Power, a)] -> [(Power, a)]
remove_0 = filter (\(_, x) -> x /= 0)

get_list :: Num a => Poly a -> [(Power, a)]
get_list (Poly l) = l


shows_poly :: Num a => Char -> Poly a -> ShowS
shows_poly symbol (Poly []) = showChar '0'
shows_poly symbol (Poly list) = shows_list list
  where
  shows_list = foldl1 (\x y -> x . showString " + " . y) . map shows_pair

  shows_pair (0, x) =
    if use_parenthesis_on_term x_str then
      showChar '(' . showString x_str . showChar ')'
    else
      showString x_str
    where
    x_str = show x

  shows_pair (i, x) =
    if x_str == "1" then
      power
    else if use_parenthesis_on_term x_str then
      showChar '(' . showString x_str . showString ")*" . power
    else
      showString x_str . showChar '*' . power
    where
    x_str = show x
    power | i == 1 = sym_str
          | otherwise = sym_str . showChar '^' . shows i
    sym_str = showChar symbol

  use_parenthesis_on_term ('-':_) = True
  use_parenthesis_on_term str   = elem '+' str

instance Num a => Num (Poly a) where

  fromInteger x = if x == 0 then Poly [] else Poly [(0, fromInteger x)]

  (+) (Poly a) (Poly b) = mk_poly (a ++ b)
  
  negate = map_poly negate
  
  (*) (Poly a) (Poly b) = mk_poly [mul_terms x y | x <- a, y <- b]
    where mul_terms (n1, x1) (n2, x2) = (n1 + n2, x1 * x2)


instance Num a => Show (Poly a) where
  showsPrec _ = shows_poly 'X'


combine_poly :: (Num a) => Poly a -> Poly a -> Poly a
combine_poly p@(Poly []) _ = p
combine_poly p@(Poly [(0, x)]) _ = p
-- ALERT: optimize the following to reuse results for (p2 ^ n) from smaller n
combine_poly (Poly l) p2 = (sum . map do_term) l where
  do_term (n, x) = map_poly (x *) (p2 ^ n)

