{-# LANGUAGE TypeFamilies #-} 

module Test where

class Collects c where
  type Elem c :: *
  empty :: c
  insert :: Elem c -> c -> c
  toList :: c-> [Elem c]

instance Eq e => Collects [e] where
  type Elem [e] = e
  empty = []
  insert e list = if elem e list then list else e : list
  toList list = list 



data family XList a

sumColl :: (Collects c, Elem c ~ Int) => c -> Int
sumColl c = sum (toList c)
