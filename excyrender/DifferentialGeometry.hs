-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module DifferentialGeometry
( DifferentialGeometry(..),
  worldToLocalDirection, localToWorldDirection
) where

import Geometry.Point
import qualified Geometry.Normal as N
import qualified Geometry.Direction as D
import qualified Geometry.Vector as V
import RealNum

--------------------------------------------------------------------------------
data DifferentialGeometry = DifferentialGeometry {
    d :: RealNum,
    poi :: Point,
    nn :: N.Normal,
    u :: RealNum,
    v :: RealNum,
    dpdu :: V.Vector
    -- shape :: Shape <-- This is as PBRT has it, but it would produce a 
    --                    recursive dependency
} deriving (Show)


worldToLocalDirection :: DifferentialGeometry -> D.Direction -> D.Direction
localToWorldDirection :: DifferentialGeometry -> D.Direction -> D.Direction

worldToLocalDirection dg dir =
  let n' = N.asVector (nn dg)
      s' = V.normalize (dpdu dg)
      t' = n' `V.cross` s'
      v' = D.asVector dir
  in D.direction (v' `V.dot` s') (v' `V.dot` n') (v' `V.dot` t')

localToWorldDirection dg dir =   
  let n'@(V.Vector nx ny nz) = N.asVector (nn dg)
      s'@(V.Vector sx sy sz) = V.normalize (dpdu dg)
      (V.Vector tx ty tz)    = n' `V.cross` s'
      (V.Vector vx vy vz)    = D.asVector dir
  in D.direction (sx*vx + nx*vy + tx*vz)
                 (sy*vx + ny*vy + ty*vz)
                 (sz*vx + nz*vy + tz*vz)


-- TODO: use a smart constructor to enforce d's positiveness
