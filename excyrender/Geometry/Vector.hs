-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Geometry.Vector (
  Vector (..),
  add, sub,
  len, len_sq,
  stretch, shrink,
  normalize,
  dot
) where

import RealNum

data Vector = Vector RealNum RealNum RealNum
              deriving (Show)

add       :: Vector -> Vector -> Vector
sub       :: Vector -> Vector -> Vector
len_sq    :: Vector -> RealNum
len       :: Vector -> RealNum
stretch   :: Vector -> RealNum -> Vector
shrink    :: Vector -> RealNum -> Vector
normalize :: Vector -> Vector
dot       :: Vector -> Vector -> RealNum

add       (Vector a b c) (Vector x y z) = Vector (a+x) (b+y) (c+z) 
sub       (Vector a b c) (Vector x y z) = Vector (a-x) (b-y) (c-z) 
len_sq    (Vector a b c)                = a*a + b*b + c*c
len                                     = sqrt . len_sq
stretch   (Vector a b c) f              = Vector (a*f) (b*f) (c*f)
shrink    (Vector a b c) f              = Vector (a/f) (b/f) (c/f)
normalize v                             = v `shrink` (len v)
dot       (Vector a b c) (Vector x y z) = a*x + b*y + c*z

