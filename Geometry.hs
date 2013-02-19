module Geometry
( Angle
) where

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

-- Vector ----------------------------------------------------------------------
data Vector t = Vector t t t
                deriving (Show)

v_add :: (Num t) => Vector t -> Vector t -> Vector t
v_sub :: (Num t) => Vector t -> Vector t -> Vector t

v_add (Vector a b c) (Vector x y z) = Vector (a+x) (b+y) (c+z) 
v_sub (Vector a b c) (Vector x y z) = Vector (a-x) (b-y) (c-z) 
