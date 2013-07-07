-- (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
-- GNU General Public License, Version 3 (a.k.a. GPLv3).
-- See COPYING in the root-folder of the excygen project folder.

module Photometry.Background.Preetham.Preetham
( preetham
) where

import Photometry.Spectrum as Spectrum
import Geometry.Ray as Ray
import Geometry.Direction as D
import RealNum

preetham :: (Ray.Ray -> Spectrum)
preetham = 
    let
        radians degrees  = degrees * (pi/180.0)
        julianDay        = 365.0/2.0 -- 1..365
        longitude        = 45  -- -90..90 -- south to north
        latitude         = 0  -- 0..360
        standardMeridian = 0
        timeOfDay        = 14.0 -- 0..24
        turbidity        = 1.9  -- 2..6 are most useful

        solarTime = timeOfDay +
                	(0.170*(sin 4.0*pi*(julianDay - 80.0)/373.0)
                     - 0.129*(sin 2.0*pi*(julianDay - 8.0)/355.0))
                	+ (standardMeridian - longitude)/15.0
        solarDeclination = (0.4093*sin(2*pi*(julianDay - 81)/368))
        solarAltitude = asin(sin(radians(latitude)) * sin(solarDeclination) -
			            cos(radians(latitude)) * cos(solarDeclination) * cos(pi*solarTime/12.0))
        
        solarAzimuth = let opp = (-cos solarDeclination) * (sin pi*solarTime/12.0)
                           adj = (-(cos (radians latitude)) * (sin solarDeclination) +
                                  (sin (radians latitude)) * (cos solarDeclination) *  (cos pi*solarTime/12.0))
                       in atan2 opp adj

        theta_s = pi / 2.0 - solarAltitude
        phi_s   = (-solarAzimuth)
    in skylight theta_s phi_s turbidity


skylight :: RealNum -> RealNum -> RealNum -> Ray -> Spectrum
skylight theta_s phi_s t (Ray.Ray _ rd) =
    if (D.v rd) < 0 then
        Spectrum.black 300 830 54
    else
        let
            d = D.direction (D.u rd)
                            (max 0.001 (D.v rd))
                            (D.w rd)
            theta = acos (D.v d)
            phi = if (abs theta) < 1e-5 then 0 else atan2 (D.w d) (D.u d)
        in skylight' theta_s phi_s t theta phi


angleBetween :: RealNum -> RealNum -> RealNum -> RealNum -> RealNum
angleBetween thetav phiv theta phi =
    let cospsi = sin(thetav) * sin(theta) * cos(phi-phiv) + cos(thetav) * cos(theta)
    in if (cospsi > 1) then 0
       else if (cospsi < -1) then pi
       else acos(cospsi)


skylight' :: RealNum -> RealNum -> RealNum -> RealNum -> RealNum -> Spectrum
skylight' theta_s phi_s t theta phi =
    let ---------------------------------------------------------------------------------
        chi = (4.0/9.0 - t/120.0) * (pi - 2.0*theta_s)
        zenith_Y = (4.0453 * t - 4.9710) * tan(chi) - 0.2155 * t + 2.4192
                   * 1000.0 -- conversion from kcd/m^2 to cd/m^2

        theta_s2 = theta_s * theta_s
        theta_s3 = theta_s2 * theta_s
        t2       = t * t

        zenith_x = (( 0.00165)*theta_s3 - 0.00374*theta_s2 + 0.00208*theta_s + 0)       * t2 +
                   ((-0.02902)*theta_s3 + 0.06377*theta_s2 - 0.03202*theta_s + 0.00394) * t  +
                   (( 0.11693)*theta_s3 - 0.21196*theta_s2 + 0.06052*theta_s + 0.25885)

        zenith_y = (( 0.00275)*theta_s3 - 0.00610*theta_s2 + 0.00316*theta_s  + 0)       * t2 +
                   ((-0.04214)*theta_s3 + 0.08970*theta_s2 - 0.04153*theta_s  + 0.00515) * t  +
                   (( 0.15346)*theta_s3 - 0.26756*theta_s2 + 0.06669*theta_s  + 0.26688)

        ---------------------------------------------------------------------------------
        perez (a,b,c,d,e) theta' gamma' = (1 + a * exp(b/cos theta')) * (1+c*exp(d*gamma') + e * (cos gamma')**2)

        perez_Y = perez ( 0.17872*t - 1.46303, 
                         -0.35540*t + 0.42749,
                         -0.02266*t + 5.32505,
                          0.12064*t - 2.57705,
                         -0.06696*t + 0.37027)
          
        perez_x = perez (-0.01925*t  - 0.25922,
                         -0.06651*t  + 0.00081,
                         -0.00041*t  + 0.21247,
                         -0.06409*t  - 0.89887,
                         -0.00325*t  + 0.04517)
          
        perez_y = perez (-0.01669*t  - 0.26078,
                         -0.09495*t  + 0.00921,
                         -0.00792*t  + 0.21023,
                         -0.04405*t  - 1.65369,
                         -0.01092*t  + 0.05291)

        gamma = angleBetween theta phi theta_s phi_s

        lumina_Y = zenith_Y * (perez_Y theta gamma) / (perez_Y 0.0 theta_s) :: RealNum
        chroma_x = zenith_x * (perez_x theta gamma) / (perez_x 0.0 theta_s) :: RealNum
        chroma_y = zenith_y * (perez_y theta gamma) / (perez_y 0.0 theta_s) :: RealNum       
    in
        let c = chromaticityToSpectrum chroma_x chroma_y
        in Spectrum.stretch (Spectrum.stretch c lumina_Y) (1.0 / Spectrum.toY c)
    -- return Y * spect / RiColorXYZV(spect).Y(); TODO!




s0Spectrum :: Spectrum
s1Spectrum :: Spectrum
s2Spectrum :: Spectrum
s0Spectrum = spectrum 300 830 [0.04,6.0,29.6,55.3,57.3,
                               61.8,61.5,68.8,63.4,65.8,
                               94.8,104.8,105.9,96.8,113.9,
                               125.6,125.5,121.3,121.3,113.5,
                               113.1,110.8,106.5,108.8,105.3,
                               104.4,100.0,96.0,95.1,89.1,
                               90.5,90.3,88.4,84.0,85.1,
                               81.9,82.6,84.9,81.3,71.9,
                               74.3,76.4,63.3,71.7,77.0,
                               65.2,47.7,68.6,65.0,66.0,
                               61.0,53.3,58.9,61.9]
s1Spectrum = spectrum 300 830 [0.02,4.5,22.4,42.0,40.6,
                               41.6,38.0,42.4,38.5,35.0,
                               43.4,46.3,43.9,37.1,36.7,
                               35.9,32.6,27.9,24.3,20.1,
                               16.2,13.2,8.6,6.1,4.2,
                               1.9,0.0,-1.6,-3.5,-3.5,
                               -5.8,-7.2,-8.6,-9.5,-10.9,
                               -10.7,-12.0,-14.0,-13.6,-12.0,
                               -13.3,-12.9,-10.6,-11.6,-12.2,
                               -10.2,-7.8,-11.2,-10.4,-10.6,
                               -9.7,-8.3,-9.3,-9.8]
s2Spectrum = spectrum 300 830 [0.0,2.0,4.0,8.5,7.8,
                               6.7,5.3,6.1,3.0,1.2,
                               -1.1,-0.5,-0.7,-1.2,-2.6,
                               -2.9,-2.8,-2.6,-2.6,-1.8,
                               -1.5,-1.3,-1.2,-1.0,-0.5,
                               -0.3,0.0,0.2,0.5,2.1,
                               3.2,4.1,4.7,5.1,6.7,
                               7.3,8.6,9.8,10.2,8.3,
                               9.6,8.5,7.0,7.6,8.0,
                               6.7,5.2,7.4,6.8,7.0,
                               6.4,5.5,6.1,6.5]

chromaticityToSpectrum :: RealNum -> RealNum -> Spectrum
chromaticityToSpectrum x y =
    let m1 = (-1.3515 - 1.7703*x +  5.9114*y)/(0.0241 + 0.2562*x - 0.7341*y)
        m2 = ( 0.03   -31.4424*x + 30.0717*y)/(0.0241 + 0.2562*x - 0.7341*y)
    in
        Spectrum.sum [s0Spectrum,
                      s1Spectrum `Spectrum.stretch` m1,
                      s2Spectrum `Spectrum.stretch` m2]
