module Geometry
( Angle
) where

-- Angle -----------------------------------------------------------------------
data Angle t = Degrees t | Radians t

degrees :: (Floating t) => Angle t -> t
degrees (Degrees val) = val
degrees (Radians val) = val * (180/pi)

radians :: (Floating t) => Angle t -> t
radians (Degrees val) = val * (pi/180)
radians (Radians val) = val
