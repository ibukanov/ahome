module A_alg where

import List (sortBy)

import Misc_Util

import NPoly

type A_term = (Integer, NPoly)

newtype APoly = APolyConst [A_term] deriving Eq

mkA :: [A_term] -> APoly
mkA = APolyConst  . filter not_0 . fuse_groups_by same_a . sortBy a_cmp
  where
    not_0 (_, n_poly) = n_poly /= 0

    a_cmp (i, n_poly1) (j, n_poly2) =
      case compare (abs i) (abs j) of
        EQ -> compare i j
       	i_j_abs_difference -> i_j_abs_difference

    same_a (i, n_poly1) (j, n_poly2) = 
      if (i == j) then Just (i, n_poly1 + n_poly2) else Nothing
    
instance Num APoly where

  fromInteger n = mkA [(0, fromInteger n)]

  APolyConst list1 + APolyConst list2 = 
    mkA (list1 ++ list2)

  APolyConst list1 * APolyConst list2 = 
    mkA [a_product x y | x <- list1, y <- list2]
  
  negate (APolyConst list) =
    (mkA . map (\ (i, n_poly) -> (i, negate n_poly))) list

--  abs (APolyConst list) = (mkA . map (\ (i, n_poly) -> (i, abs n_poly))) list


instance Show APoly where
  showsPrec n (APolyConst list) = showsPrec n list

(<->) :: Num a => a -> a -> a
(<->) x y = x * y - y * x

a_product :: A_term -> A_term -> A_term
a_product (i, f_N) (j, g_N) = (i + j, pi_value) where
  pi_value
    | (i >= 0 && j >= 0) = f_N * (combine_npoly g_N (nshift i))
    | (i <= 0 && j <= 0) = (combine_npoly f_N (nshift (-j))) * g_N
    | (i >= 0 && j <= 0 && i + j >= 0)
      = f_N * (combine_npoly g_N (nshift (i + j))) * (prod (i + j + 1) i)
    | (i >= 0 && j <= 0 && i + j <= 0)
      = (combine_npoly f_N (nshift (-i - j))) * g_N * (prod (-j - i + 1) (-j))
    | (i <= 0 && j >= 0 && i + j >= 0)
      = (combine_npoly f_N (nshift i)) * (combine_npoly g_N (nshift i)) 
        * (prod (i + 1) 0)
    | (i <= 0 && j >= 0 && i + j <= 0)
      = (combine_npoly f_N (nshift (-j))) * (combine_npoly g_N (nshift (-j))) 
        * (prod (-j + 1) 0)
  nshift i = mk_npoly [ (0, fromInteger i), (1, 1) ]
  prod from to 
    | from > to = 1
    | from <= to = (prod from (to - 1)) * (cN + fromInteger to)