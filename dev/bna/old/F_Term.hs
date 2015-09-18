module F_Term where

import List(sort)

type Symbol = String

data M_Term = M_Term Double [Symbol]

m_normalize :: M_Term -> M_Term
m_normalize (M_Term n list) = M_Term n (sort list)

instance Eq M_Term where
  (==) (M_Term n1 list1) (M_Term n2 list2) = do_eq n1 n2 list1 list2 where
     do_eq 0 0 _ _ = True
     do_eq 0 n _ _ = False
     do_eq n 0 _ _ = False
     do_eq n1 n2 list1 list2 = n1 == n2 && sort list1 == sort list2
	  
instance Ord M_Term where
  compare (M_Term n1 list1) (M_Term n2 list2) 
    = cmp_number_part n1 n2 (compare (sort list1) (sort list2)) where
      cmp_number_part _ _ LT = LT
      cmp_number_part _ _ GT = GT
      cmp_number_part n1 n2 EQ = compare n1 n2

instance Show M_Term where
  showsPrec _ (M_Term n list) s = shows_m_term n list s where
     shows_m_term 0 list s = '0' : s
     shows_m_term 1 (x:xs) s = shows x (show_symbols xs s)
     shows_m_term n list s = shows_short_float n (show_symbols list s)
     show_symbols [] s = s 
     show_symbols (x:xs) s = '*' : shows x (show_symbols xs s)
  
  
newtype F_Term = F_Term [M_Term]

instance Eq F_Term where
  (==) (F_Term list1) (F_Term list2) = list1 == list2

instance Show F_Term where
  showsPrec _ (F_Term list) s = shows_list list s where
    shows_list [] s = s
    shows_list [x] s = shows x s
    shows_list (x:xs) s = shows x (" + " ++ shows_list xs s) 



shows_short_float :: Double->String->String
shows_short_float x s = let i = floor x in
  if fromInteger i == x then shows i s else shows x s

normalize_m_terms :: [M_Term] -> [M_Term]
normalize_m_terms [] = []
normalize_m_terms list = let (x:xs) = sort list in process x xs where
  process :: M_Term -> [M_Term] -> [M_Term]
  process accumulator [] = [accumulator]
  process accumulator@(M_Term ) (x:xs) = [accumulator]
  
  

normalize_m_terms list = normalize_sorted (sort list) where
  normalize_sorted [] = []
  normalize_sorted [] = []
 
