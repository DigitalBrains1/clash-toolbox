{-# LANGUAGE RankNTypes #-}
{-
 - Copyright (c) 2020 QBayLogic B.V.
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 - 1. Redistributions of source code must retain the above copyright notice,
 - this list of conditions and the following disclaimer.
 -
 - 2. Redistributions in binary form must reproduce the above copyright notice,
 - this list of conditions and the following disclaimer in the documentation
 - and/or other materials provided with the distribution.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 - AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 - IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 - LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 - CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 - SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 - INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 - CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 - ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 - POSSIBILITY OF SUCH DAMAGE.
 -}

module Test.Toolbox.ClockScaler.Analysis
    ( module Test.Toolbox.ClockScaler.Analysis
    , module Toolbox.ClockScaler
    ) where

import Clash.Prelude
import qualified Prelude as P

import Toolbox.ClockScaler

createDomain vSystem{vName="Test3000", vPeriod=3000}

{-
 - Output the clock pulses where a clock scaler is True, and the difference
 - with the previous pulse.
 -
 - Usage examples:
 -
 - >>> P.take 5 $ pulses $ avgRatio d1 d4 (pure Run :: Signal System State)
 - [(4,0),(8,4),(12,4),(16,4),(20,4)]
 -
 - >>> cs = avgPeriod (SNat :: SNat 11000) (pure Run :: Signal Test3000 State)
 - >>> P.take 20 $ pulses cs
 - [(4,0),(8,4),(11,3),(15,4),(19,4),(22,3),(26,4),(30,4),(33,3),(37,4)
 - ,(41,4),(44,3),(48,4),(52,4),(55,3),(59,4),(63,4),(66,3),(70,4),(74,4)]
 -
 -}
pulses
    :: KnownDomain dom
    => (HiddenClockResetEnable dom => Signal dom Bool)
    -> [(Integer, Integer)]

pulses cs
    = P.zip pulses' $ P.map (uncurry (-)) $ P.zip pulses'
    $ P.head pulses' : pulses'
    where
        pulses' = P.map fst $ P.filter snd $ P.zip [0..] $ sample cs

{-
 - Check that a clock scaler is periodic.
 -
 - Usage example:
 -
 - >>> cs = avgRatio d3 d11 (pure Run :: Signal System State)
 - >>> P.take 20 $ pulses cs
 - [(4,0),(8,4),(11,3),(15,4),(19,4),(22,3),(26,4),(30,4),(33,3),(37,4)
 - ,(41,4),(44,3),(48,4),(52,4),(55,3),(59,4),(63,4),(66,3),(70,4),(74,4)]
 -
 - A clock scaler with this ratio ought to have a period of (lcm 3 11), or 33
 - clock ticks. We see that there is a pulse at cycle 37, but is it periodic
 - after that? If it does repeat with period 33, checkPeriod 33 will output a
 - stream of zeroes.
 -
 - >>> P.take 10000 (checkPeriod 33 cs) == P.replicate 10000 0
 - True
 -
 - It does for the first 10,000 pulses. Should be okay.
 -}
checkPeriod
    :: KnownDomain dom
    => Integer
    -> (HiddenClockResetEnable dom => Signal dom Bool)
    -> [Integer]

checkPeriod period cs
    = P.map (uncurry (-)) $ P.zip pDiffs $ P.drop periodTicks pDiffs
    where
        ((firstP,_):ps) = pulses cs
        pDiffs = P.map snd ps
        periodTicks =
            if P.last firstPeriod /= period + firstP then
                error "There was no tick at the specified period"
            else
                P.length firstPeriod
        firstPeriod = P.takeWhile (<= period + firstP) $ P.map fst ps


{-
 - These functions were used to develop the constraints that would compute
 - multiplier and divider. They might be of use when developing something
 - similar.
 -}
printParamsSystem = printParams @System (pure ())
printParamsTest3000 = printParams @Test3000 (pure ())

printParams
    :: ( KnownDomain dom
       , DomainPeriod dom ~ clkPeriod
       , KnownNat clkPeriod
       , KnownNat period
       , 1 <= clkPeriod
       , 1 <= period
       )
    => Signal dom ()
    -> SNat period
    -> IO ()
printParams = withSNat $ withSNat printParams'

printParams'
    :: ( KnownDomain dom
       , DomainPeriod dom ~ clkPeriod
       , KnownNat clkPeriod
       , KnownNat period
       , KnownNat common
       , KnownNat mlt
       , KnownNat dv
       , 1 <= clkPeriod
       , 1 <= period
       , LCM clkPeriod period ~ common
       , Div common clkPeriod ~ dv
       , Div common period ~ mlt
       )
    => SNat mlt
    -> SNat dv
    -> Signal dom ()
    -> SNat period
    -> IO ()

printParams' n1 n2 _ _ = print n1 >> print n2
