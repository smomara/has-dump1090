module Aircraft.CPR
  ( decodeCPR
  , CPRPosition (..)
  ) where

import Data.Fixed (mod')

-- | Position decoded from CPR coordinates
data CPRPosition = CPRPosition
  { latitude :: !Double
  , longitude :: !Double
  }
  deriving Show

-- | Constants for CPR decoding
airDLat0, airDLat1 :: Double
airDLat0 = 360.0 / 60 -- Even latitude zone size
airDLat1 = 360.0 / 59 -- Odd latitude zone size

-- | Decode CPR position from even/odd frame coordinates and timestamps
decodeCPR :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe CPRPosition
decodeCPR evenLat evenLon oddLat oddLon evenTime oddTime =
  -- Only decode if messages are less than 10 seconds apart
  if abs (evenTime - oddTime) > 10000
    then Nothing
    else decodePosition
 where
  decodePosition :: Maybe CPRPosition
  decodePosition = do
    -- Convert coordinates to floating point
    let lat0 = fromIntegral evenLat / 131072.0
        lat1 = fromIntegral oddLat / 131072.0
        lon0 = fromIntegral evenLon / 131072.0
        lon1 = fromIntegral oddLon / 131072.0

    -- Compute latitude index
    let j :: Integer
        j = floor $ ((59 * lat0 - 60 * lat1) / 131072.0) + 0.5

    -- Compute final latitudes for both odd and even frames
    let rlat0 = airDLat0 * (modFloat (fromIntegral j, 60) + lat0)
        rlat1 = airDLat1 * (modFloat (fromIntegral j, 59) + lat1)

    -- Adjust latitudes for southern hemisphere
    let rlat0' = if rlat0 >= 270 then rlat0 - 360 else rlat0
        rlat1' = if rlat1 >= 270 then rlat1 - 360 else rlat1

    -- Check that both are in same latitude zone
    if nlFunction rlat0' /= nlFunction rlat1'
      then Nothing
      else do
        -- Compute final position based on most recent frame
        if evenTime > oddTime
          then do
            -- Use even frame data
            let nl = nlFunction rlat0'
                ni = max 1 (nFunction rlat0' 0)
                m :: Integer
                m =
                  floor
                    $ ( ( lon0 * fromIntegral (nl - 1)
                            - lon1 * fromIntegral nl
                        )
                          / 131072.0
                      )
                      + 0.5
                dLon = 360.0 / fromIntegral ni
                lon = dLon * (modFloat (fromIntegral m, ni) + lon0)
                lonAdj = if lon > 180 then lon - 360 else lon
            Just
              $ CPRPosition
                { latitude = rlat0'
                , longitude = lonAdj
                }
          else do
            -- Use odd frame data
            let nl = nlFunction rlat1'
                ni = max 1 (nFunction rlat1' 1)
                m :: Integer
                m =
                  floor
                    $ ( ( lon0 * fromIntegral (nl - 1)
                            - lon1 * fromIntegral nl
                        )
                          / 131072.0
                      )
                      + 0.5
                dLon = 360.0 / fromIntegral ni
                lon = dLon * (modFloat (fromIntegral m, ni) + lon1)
                lonAdj = if lon > 180 then lon - 360 else lon
            Just
              $ CPRPosition
                { latitude = rlat1'
                , longitude = lonAdj
                }

-- | Positive modulo operation
modFloat :: (Double, Int) -> Double
modFloat (a, b) = a `mod'` fromIntegral b

-- | NL function to determine number of longitude zones
nlFunction :: Double -> Int
nlFunction lat =
  let abslat = abs lat
  in if abslat < 10.47047130
       then 59
       else
         if abslat < 14.82817437
           then 58
           else
             if abslat < 18.18626357
               then 57
               else
                 if abslat < 21.02939493
                   then 56
                   else
                     if abslat < 23.54504487
                       then 55
                       else
                         if abslat < 25.82924707
                           then 54
                           else
                             if abslat < 27.93898710
                               then 53
                               else
                                 if abslat < 29.91135686
                                   then 52
                                   else
                                     if abslat < 31.77209708
                                       then 51
                                       else
                                         if abslat < 33.53993436
                                           then 50
                                           else
                                             if abslat < 35.22899598
                                               then 49
                                               else
                                                 if abslat < 36.85025108
                                                   then 48
                                                   else
                                                     if abslat < 38.41241892
                                                       then 47
                                                       else
                                                         if abslat < 39.92256684
                                                           then 46
                                                           else
                                                             if abslat < 41.38651832
                                                               then 45
                                                               else
                                                                 if abslat < 42.80914012
                                                                   then 44
                                                                   else
                                                                     if abslat < 44.19454951
                                                                       then 43
                                                                       else
                                                                         if abslat < 45.54626723
                                                                           then 42
                                                                           else
                                                                             if abslat < 46.86733252
                                                                               then 41
                                                                               else
                                                                                 if abslat < 48.16039128
                                                                                   then 40
                                                                                   else
                                                                                     if abslat < 49.42776439
                                                                                       then 39
                                                                                       else
                                                                                         if abslat < 50.67150166
                                                                                           then 38
                                                                                           else
                                                                                             if abslat < 51.89342469
                                                                                               then 37
                                                                                               else
                                                                                                 if abslat < 53.09516153
                                                                                                   then 36
                                                                                                   else
                                                                                                     if abslat < 54.27817472
                                                                                                       then 35
                                                                                                       else
                                                                                                         if abslat < 55.44378444
                                                                                                           then 34
                                                                                                           else
                                                                                                             if abslat < 56.59318756
                                                                                                               then 33
                                                                                                               else
                                                                                                                 if abslat < 57.72747354
                                                                                                                   then 32
                                                                                                                   else
                                                                                                                     if abslat < 58.84763776
                                                                                                                       then 31
                                                                                                                       else
                                                                                                                         if abslat < 59.95459277
                                                                                                                           then 30
                                                                                                                           else
                                                                                                                             if abslat < 61.04917774
                                                                                                                               then 29
                                                                                                                               else
                                                                                                                                 if abslat < 62.13216659
                                                                                                                                   then 28
                                                                                                                                   else
                                                                                                                                     if abslat < 63.20427479
                                                                                                                                       then 27
                                                                                                                                       else
                                                                                                                                         if abslat < 64.26616523
                                                                                                                                           then 26
                                                                                                                                           else
                                                                                                                                             if abslat < 65.31845310
                                                                                                                                               then 25
                                                                                                                                               else
                                                                                                                                                 if abslat < 66.36171008
                                                                                                                                                   then 24
                                                                                                                                                   else
                                                                                                                                                     if abslat < 67.39646774
                                                                                                                                                       then 23
                                                                                                                                                       else
                                                                                                                                                         if abslat < 68.42322022
                                                                                                                                                           then 22
                                                                                                                                                           else
                                                                                                                                                             if abslat < 69.44242631
                                                                                                                                                               then 21
                                                                                                                                                               else
                                                                                                                                                                 if abslat < 70.45451075
                                                                                                                                                                   then 20
                                                                                                                                                                   else
                                                                                                                                                                     if abslat < 71.45986473
                                                                                                                                                                       then 19
                                                                                                                                                                       else
                                                                                                                                                                         if abslat < 72.45884545
                                                                                                                                                                           then 18
                                                                                                                                                                           else
                                                                                                                                                                             if abslat < 73.45177442
                                                                                                                                                                               then 17
                                                                                                                                                                               else
                                                                                                                                                                                 if abslat < 74.43893416
                                                                                                                                                                                   then 16
                                                                                                                                                                                   else
                                                                                                                                                                                     if abslat < 75.42056257
                                                                                                                                                                                       then 15
                                                                                                                                                                                       else
                                                                                                                                                                                         if abslat < 76.39684391
                                                                                                                                                                                           then 14
                                                                                                                                                                                           else
                                                                                                                                                                                             if abslat < 77.36789461
                                                                                                                                                                                               then 13
                                                                                                                                                                                               else
                                                                                                                                                                                                 if abslat < 78.33374083
                                                                                                                                                                                                   then 12
                                                                                                                                                                                                   else
                                                                                                                                                                                                     if abslat < 79.29428225
                                                                                                                                                                                                       then 11
                                                                                                                                                                                                       else
                                                                                                                                                                                                         if abslat < 80.24923213
                                                                                                                                                                                                           then 10
                                                                                                                                                                                                           else
                                                                                                                                                                                                             if abslat < 81.19801349
                                                                                                                                                                                                               then 9
                                                                                                                                                                                                               else
                                                                                                                                                                                                                 if abslat < 82.13956981
                                                                                                                                                                                                                   then 8
                                                                                                                                                                                                                   else
                                                                                                                                                                                                                     if abslat < 83.07199445
                                                                                                                                                                                                                       then 7
                                                                                                                                                                                                                       else
                                                                                                                                                                                                                         if abslat < 83.99173563
                                                                                                                                                                                                                           then 6
                                                                                                                                                                                                                           else
                                                                                                                                                                                                                             if abslat < 84.89166191
                                                                                                                                                                                                                               then 5
                                                                                                                                                                                                                               else
                                                                                                                                                                                                                                 if abslat < 85.75541621
                                                                                                                                                                                                                                   then 4
                                                                                                                                                                                                                                   else
                                                                                                                                                                                                                                     if abslat < 86.53536998
                                                                                                                                                                                                                                       then 3
                                                                                                                                                                                                                                       else
                                                                                                                                                                                                                                         if abslat < 87.00000000
                                                                                                                                                                                                                                           then 2
                                                                                                                                                                                                                                           else 1

-- | Calculates the number of longitude zones at a given latitude
nFunction :: Double -> Int -> Int
nFunction lat isOdd =
  let nl = nlFunction lat - isOdd
  in max 1 nl
