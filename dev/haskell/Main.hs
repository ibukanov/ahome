module Main where

data GetResult a source = Stop | More a (source a)

class Source source where
  draw :: source a -> GetResult a source

class Sink sink where
  put :: a -> sink a -> sink a

sfoldr :: Source source => (a->b->b) -> b -> source a -> b
sfoldr f inital source = case draw source of
  Stop -> inital
  More value source2 -> f value (sfoldr f inital source2) 

sfoldl :: Source source => (a->b->a) -> a -> source b -> a
sfoldl f inital source = case draw source of
  Stop -> inital
  More value source2 -> sfoldl f (f inital value) source2

sfoldl' :: Source source => (a->b->a) -> a -> source b -> a
sfoldl' f inital source = case draw source of
  Stop -> inital
  More value source2 -> let x = f inital value in seq x $ sfoldl' f x source2



toList :: (Source source) => source a -> [a]
toList = sfoldr (:) []

instance Source [] where
  draw [] = Stop
  draw (x:xs) = More x xs

instance Sink [] where
  put x xs = (x : xs)




data TransformedSource source from to = TransformedSource (from->to) (source from)

instance Source source => Source (TransformedSource source from) where
  draw (TransformedSource f source) = case draw source of
    Stop -> Stop
    More value source2 -> More (f value) (TransformedSource f source2)

twice = (*) 2

main = do
  print $ toList (TransformedSource twice [1,2,3,4,5])
  print $ sfoldl' (+) 0 [1..1000000]

