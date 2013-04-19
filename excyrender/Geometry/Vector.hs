-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Geometry.Vector (
  Vector (..),
  add, sub,
  len, len_sq,
  stretch, shrink,
  normalize
) where

data Vector t = Vector t t t
                deriving (Show)

add       :: (Num t)        => Vector t -> Vector t -> Vector t
sub       :: (Num t)        => Vector t -> Vector t -> Vector t
len_sq    :: (Num t)        => Vector t -> t
len       :: (Floating t)   => Vector t -> t
stretch   :: (Num t)        => Vector t -> t -> Vector t
shrink    :: (Fractional t) => Vector t -> t -> Vector t
normalize :: (Floating t)   => Vector t -> Vector t

add       (Vector a b c) (Vector x y z) = Vector (a+x) (b+y) (c+z) 
sub       (Vector a b c) (Vector x y z) = Vector (a-x) (b-y) (c-z) 
len_sq    (Vector a b c)                = a*a + b*b + c*c
len                                     = sqrt . len_sq
stretch   (Vector a b c) f              = Vector (a*f) (b*f) (c*f)
shrink    (Vector a b c) f              = Vector (a/f) (b/f) (c/f)
normalize v                             = v `shrink` (len v)


