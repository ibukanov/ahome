module NPoly (
  NPoly, cN,cN2, cN3, cN4,  mk_npoly, list_to_npoly,
  map_npoly,
  shows_npoly,
  combine_npoly	
) where

import List(sort, sortBy)

import Misc_Util

default ()

type Power = Integer
type Coef = Double
type NList = [(Power, Coef)]

data NPoly = NPoly NList deriving Eq

mk_npoly :: NList -> NPoly
mk_npoly [] = NPoly []
mk_npoly list
  = (NPoly . remove_0 . fuse_groups_by fuser . sortBy sorter) list
  where
  fuser (n1, x1) (n2, x2) = if n1 == n2 then Just (n1, x1 + x2) else Nothing
  -- Due to nature of tuple sorting sort == sortBy sorter but I do not want
  -- to depend on it
  sorter (n1, _) (n2, _) = compare n1 n2
  
cN = NPoly [(1, 1)]
cN2 = NPoly [(2, 1)]
cN3 = NPoly [(3, 1)]
cN4 = NPoly [(4, 1)]

list_to_npoly :: [Coef] -> NPoly
list_to_npoly = NPoly . (zip [0 ..])
    
map_npoly :: (Coef -> Coef) -> NPoly -> NPoly
map_npoly f = NPoly . remove_0 . map (\(n, x) -> (n, f x)) . get_list

remove_0 :: NList -> NList
remove_0 = filter (\(_, x) -> x /= 0)

get_list :: NPoly -> NList
get_list (NPoly l) = l


shows_npoly :: NPoly -> ShowS
shows_npoly (NPoly []) = showChar '0'
shows_npoly (NPoly list) = shows_list list
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
    if x == 1 then
      power
    else if use_parenthesis_on_term x_str then
      showChar '(' . showString x_str . showString ")*" . power
    else
      showString x_str . showChar '*' . power
    where
    x_str = show x
    power | i == 1 = showN
          | otherwise = showN . showChar '^' . shows i
    showN = showChar 'N'

  use_parenthesis_on_term ('-':_) = True
  use_parenthesis_on_term str   = elem '+' str

instance Num NPoly where

  fromInteger x = if x == 0 then NPoly [] else NPoly [(0, fromInteger x)]

  (+) (NPoly a) (NPoly b) = mk_npoly (a ++ b)
  
  negate = map_npoly negate
  
  (*) (NPoly a) (NPoly b) = mk_npoly [mul_terms x y | x <- a, y <- b]
    where mul_terms (n1, x1) (n2, x2) = (n1 + n2, x1 * x2)


instance Show NPoly where
  showsPrec _ = shows_npoly


combine_npoly :: NPoly -> NPoly -> NPoly
combine_npoly (NPoly list) poly2 = mk_npoly (do_it list) where
  do_it [] = []
  do_it (zero_power_term@(0, _) : rest) = zero_power_term : do_it rest
  do_it list = helper 0 1 list
-- ALERT: optimize the following to reuse results for (p2 ^ n) in
-- p2 ^ (power - last_power) 
  helper last_power poly2_in_last_power [] = []
  helper last_power poly2_in_last_power ((power, x) : rest) | power > last_power
    = let poly2_in_power = poly2_in_last_power * (poly2 ^ (power - last_power))
      in get_list (map_npoly ((*) x) poly2_in_power) 
      	++ helper power poly2_in_power rest 
  

