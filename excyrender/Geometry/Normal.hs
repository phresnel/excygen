-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Geometry.Normal (
  Normal, normal
) where

import Geometry.Vector as V


data Normal t = Normal t t t
                deriving (Show)

normal :: (Floating t) => t -> t -> t -> Normal t

normal a b c = 
    let normal_from_vec (V.Vector x y z) = Normal x y z
    in  normal_from_vec $ V.normalize $ V.Vector a b c


