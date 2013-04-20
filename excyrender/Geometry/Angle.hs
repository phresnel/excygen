-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Geometry.Angle (
  Angle(..), degrees, radians, as_degrees, as_radians
) where

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




