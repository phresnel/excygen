-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Geometry.Geometry
( Angle(..), degrees, radians, as_degrees, as_radians
, Normal, normal
, Direction, direction, d_stretch, d_u, d_v, d_w
, Ray(..), ray, ray_origin, ray_direction, ray_point
) where


import Geometry.Vector as V
import Geometry.Point as P


-- Angle -----------------------------------------------------------------------
data Angle t = Degrees t | Radians t

instance (Show t) => Show (Angle t) where
    show (Degrees f) = show f ++ "°"
    show (Radians f) = show f ++ " rad"

degrees    :: (Floating t) => Angle t -> t
radians    :: (Floating t) => Angle t -> t
as_degrees :: (Floating t) => (Angle t) -> (Angle t)
as_radians :: (Floating t) => (Angle t) -> (Angle t)

degrees (Degrees val) = val
degrees (Radians val) = val * (180/pi)
radians (Degrees val) = val * (pi/180)
radians (Radians val) = val
as_degrees angle = Degrees $ degrees angle 
as_radians angle = Radians $ radians angle 



-- Normal ----------------------------------------------------------------------
data Normal t = Normal t t t
                deriving (Show)

normal :: (Floating t) => t -> t -> t -> Normal t

normal a b c = 
    let normal_from_vec (V.Vector x y z) = Normal x y z
    in  normal_from_vec $ V.normalize $ V.Vector a b c



-- Direction -------------------------------------------------------------------
data Direction t = Direction t t t
                   deriving (Show)

direction :: (Floating t) => t -> t -> t -> Direction t
d_stretch :: (Num t)      => Direction t -> t -> Vector t
d_u       :: Direction t -> t
d_v       :: Direction t -> t
d_w       :: Direction t -> t

direction a b c = 
    let direction_from_vec (V.Vector x y z) = Direction x y z
    in  direction_from_vec $ V.normalize $ V.Vector a b c

d_stretch (Direction a b c) f = Vector (a*f) (b*f) (c*f)
d_u (Direction f _ _) = f
d_v (Direction _ f _) = f
d_w (Direction _ _ f) = f



-- Ray -------------------------------------------------------------------------
data Ray t = Ray (P.Point t) (Direction t)
             deriving (Show)

ray           :: P.Point t -> Direction t -> Ray t
ray_direction :: Ray t -> Direction t
ray_origin    :: Ray t -> P.Point t
ray_point     :: (Num t, Ord t) => Ray t -> t -> P.Point t

ray origin direction = Ray origin direction
ray_direction (Ray _ direction) = direction
ray_origin    (Ray origin _)    = origin

ray_point (Ray point direction) f
    | f<0       = error "ray_point undefined for negative f"
    | otherwise = P.add point (d_stretch direction f)
