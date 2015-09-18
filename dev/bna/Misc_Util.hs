module Misc_Util (
  fuse_groups_by
) where

fuse_groups_by :: (a -> a -> Maybe a) -> [a] -> [a]
fuse_groups_by _ [] = []
fuse_groups_by test (x:xs) = do_fuse x xs where
  do_fuse x [] = [x]
  do_fuse x (y:ys) = case (test x y) of
    Nothing -> x : do_fuse y ys
    Just a -> do_fuse a ys
