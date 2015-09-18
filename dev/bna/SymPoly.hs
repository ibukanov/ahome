module SymPoly (
  Symbol, NumCoef,
  SymPoly,
  spt, spt0, spt1, spt2, spt3
) where

import List(sort, sortBy)

import Misc_Util

type Symbol = String
type NumCoef = Integer

type Term_symbol_pair = (Integer,  Symbol)

data Term = Term NumCoef [Term_symbol_pair]

newtype SymPoly = MkSymPoly [Term]

term_to_poly :: Term -> SymPoly
term_to_poly (Term 0 _) = MkSymPoly []
term_to_poly term = MkSymPoly [term]

sym_poly :: [Term] -> SymPoly
sym_poly [] = MkSymPoly []
sym_poly [term] = term_to_poly term
sym_poly list 
  = (MkSymPoly . filter not_0 . fuse_groups_by fuser . sort) list
  where 
    not_0 (Term n _) = n /= 0
    fuser (Term n1 symbols1) (Term n2 symbols2) 
          = if symbols1 == symbols2 then Just (Term (n1 + n2) symbols1)
            else Nothing

spt :: NumCoef -> [Term_symbol_pair] -> SymPoly
spt n symbols = term_to_poly (mkterm n symbols)

spt0 :: NumCoef -> SymPoly
spt0 n = term_to_poly (mkterm n [])

spt1 :: NumCoef -> Term_symbol_pair -> SymPoly
spt1 n sym = term_to_poly (mkterm n [sym])

spt2 :: NumCoef -> Term_symbol_pair -> Term_symbol_pair -> SymPoly
spt2 n sym1 sym2 = term_to_poly (mkterm n [sym1, sym2])

spt3 :: NumCoef -> Term_symbol_pair -> Term_symbol_pair -> Term_symbol_pair 
         -> SymPoly
spt3 n sym1 sym2 sym3 = term_to_poly (mkterm n [sym1, sym2, sym3])

shows_sym_poly :: SymPoly -> ShowS
shows_sym_poly (MkSymPoly []) = showChar '0'
shows_sym_poly (MkSymPoly (x:xs)) = shows_term x . (foldr fuser id xs) where
  fuser :: Term -> ShowS -> ShowS
  fuser term f = showString " + " . shows_term term . f


instance Show SymPoly where
  showsPrec _ spoly = shows_sym_poly spoly

instance Eq SymPoly where
  (==) (MkSymPoly x) (MkSymPoly y) = x == y

instance Ord SymPoly where
  compare (MkSymPoly x) (MkSymPoly y) = compare x y


instance Eq Term where
  (==) (Term n1 list1) (Term n2 list2) = n1 == n2 && list1 == list2

instance Ord Term where
  compare (Term n1 list1) (Term n2 list2) = 
    case (compare list1 list2) of
      EQ -> compare n1 n2
      list_difference -> list_difference


instance Num SymPoly where

  fromInteger 0 = MkSymPoly []
  fromInteger n = MkSymPoly [Term n []]

  (+) (MkSymPoly a) (MkSymPoly b) = sym_poly (a ++ b)
  (-) (MkSymPoly a) (MkSymPoly b) = sym_poly (a ++ map neg_term b)

  (*) (MkSymPoly a) (MkSymPoly b) = sym_poly [ mul_terms x y | x <- a, y <- b]
    where mul_terms (Term n1 l1) (Term n2 l2) = mkterm (n1 + n2) (l1 ++ l2)
  
  -- optimization to bypass sym_poly
  negate (MkSymPoly a) = MkSymPoly (map neg_term a)

  abs (MkSymPoly a) = MkSymPoly (map (\ (Term n l) -> Term (abs n) l) a)
  


-- mkterm is the ONLY way to construct Term as they insure internal consistancy

mkterm :: NumCoef -> [Term_symbol_pair] -> Term

mkterm 0 _ = Term 0 []
mkterm n list = Term n (normalize list) where
  normalize = filter non_zero_power . fuse_groups_by fuser . sortBy same_symbol
  non_zero_power (power, _) = power /= 0
  same_symbol (_, symbol_1) (_, symbol_2) = compare symbol_1 symbol_2
  fuser (power_1, symbol_1) (power_2, symbol_2)
        = if symbol_1 == symbol_2 then Just (power_1 + power_2, symbol_1)
          else Nothing

--neg_term :: Term -> Term
neg_term (Term n l) = Term (-n) l

shows_term :: Term -> ShowS
shows_term (Term 0 _) = shows '0'
shows_term (Term n []) = shows n
shows_term (Term 1 symbols) = shows_symbols symbols
shows_term (Term n symbols) = shows n . showChar '*' . shows_symbols symbols

shows_symbols :: [Term_symbol_pair] -> ShowS
shows_symbols [] = id
shows_symbols [x] = shows_symbol x
shows_symbols (x:xs) = shows_symbol x . (foldr fuser id xs) where
  fuser :: Term_symbol_pair -> ShowS -> ShowS
  fuser symbol_pair f = showChar '*' . shows_symbol symbol_pair . f

shows_symbol :: Term_symbol_pair -> ShowS
shows_symbol (0, _) = showChar '0'
shows_symbol (1, symbol) = showString symbol
shows_symbol (n, symbol) = showChar '(' . showString symbol . 
                             showChar '^' . shows n . showChar ')' 

