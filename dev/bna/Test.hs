module Test where

import A_alg
import Poly
import SymPoly

mk_h_symbol symbol 
  = (mkA [(1, mk_poly [spt1 1 (1, symbol)]), 
          (-1, mk_poly [spt1 1 (-1, symbol)])]) ^ 4


h1 = mk_h_symbol "e1"
h2 = mk_h_symbol "e2"
h3 = mk_h_symbol "e3"
h4 = mk_h_symbol "e4"
