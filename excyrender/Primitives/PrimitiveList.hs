-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Primitives.PrimitiveList
( primitiveList
) where

import qualified Geometry.Ray as R
import qualified Geometry.Point as P
import qualified Intersection as I
import qualified Primitives.Primitive as Pr



---------------------------------------------------------------------------------------------------

primitiveList :: [Pr.Primitive] -> Pr.Primitive



---------------------------------------------------------------------------------------------------

primitiveList primitives = 
        Pr.Primitive { 
            Pr.intersect = isect primitives,
            Pr.occludes = occl primitives
        }



---------------------------------------------------------------------------------------------------

isect     :: [Pr.Primitive] -> R.Ray -> Maybe I.Intersection
occl      :: [Pr.Primitive] -> P.Point -> P.Point -> Bool

isect [] _ = Nothing

isect (x:xs) ray =
    let other' = isect xs ray
    in case Pr.intersect x ray of
     Just current -> case other' of
                       Just other -> if I.distance current < I.distance other
                                     then Just current
                                     else other'
                       Nothing -> Just current
     Nothing -> other'


occl [] _ _ = False
occl (x:xs) p q 
   | Pr.occludes x p q = True
   | otherwise = occl xs p q

