{-
 - Copyright (c) 2015, 2017 Peter Lebbing <peter@digitalbrains.com>
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

module Toolbox.ClockScaler
    ( avgPeriod
    , avgRatio
    , State(..)
    , computeParams
    ) where

import Clash.Prelude
import qualified Prelude as P

{-
 - Control a scaler.
 -
 - Stop:  pause the clock scaler, continuing where it left off once
 -        transitioned back to Run.
 - Run:   Produce clock ticks
 - Clear: Pause and restart the clock scaler. The first tick will be a full
 -        output period after transitioning back to Run.
 -}
data State = Stop | Run | Clear
    deriving (Eq, Show, Generic, NFDataX)

{-
 - Scale a clock frequency as accurately as possible
 -
 - Outputs True once every "tick" of the desired frequency by counting and
 - scaling the system clock.
 -
 - The first argument is the desired period of the scaled clock.
 -
 - At reset or after Clear, there will be a full output period of False before
 - the first True tick. In other words, it is as if a tick had just been
 - produced in the previous input clock period.
 -}
avgPeriod
    :: ( HiddenClockResetEnable dom
       , DomainPeriod dom ~ clkPeriod
       , KnownNat clkPeriod
       , KnownNat period
       , KnownNat (clkPeriod + 1)
       , 1 <= clkPeriod
       , 2 <= period
       , (clkPeriod + 1) <= period
       )
    => SNat period
    -> Signal dom State
    -> Signal dom Bool

avgPeriod = withSNat $ withSNat computeRatio
    where
        computeRatio
            :: ( HiddenClockResetEnable dom
               , DomainPeriod dom ~ clkPeriod
               , KnownNat clkPeriod
               , KnownNat period
               , KnownNat (clkPeriod + 1)
               , KnownNat common
               , KnownNat mlt
               , KnownNat dv
               , 1 <= clkPeriod
               , 2 <= period
               , (clkPeriod + 1) <= period
               , LCM clkPeriod period ~ common
               , Div common clkPeriod ~ dv
               , Div common period ~ mlt
               )
            => SNat mlt
            -> SNat dv
            -> SNat period
            -> Signal dom State
            -> Signal dom Bool

        computeRatio mlt dv _ = avgRatio' mlt dv

{-
 - Scale a clock frequency as accurately as possible, by directly specifying
 - the desired ratio of the clocks.
 -}
avgRatio
    :: ( HiddenClockResetEnable dom
       , KnownNat mlt
       , KnownNat dv
       , KnownNat (mlt + 1)
       , 2 <= dv
       , 1 <= mlt
       , (mlt + 1) <= dv
       )
    => SNat mlt
    -> SNat dv
    -> Signal dom State
    -> Signal dom Bool
avgRatio = avgRatio'

{-
 - Because the typing system cannot prove the constraints given in avgRatio
 - above when type checking avgPeriod, we have to drop those constraints. But
 - we know them to hold also for avgPeriod, and they are required for proper
 - compilation and operation. So: don't use avgRatio' directly. Use either
 - avgPeriod or avgRatio.
 -}
avgRatio'
    :: ( HiddenClockResetEnable dom
       , KnownNat mlt
       , KnownNat dv
       )
    => SNat mlt
    -> SNat dv
    -> Signal dom State
    -> Signal dom Bool
avgRatio' mlt dv =
    mealy (avgRatioT mlt dv) 0

avgRatioT
    :: (KnownNat mlt, KnownNat dv)
    => SNat mlt
    -> SNat dv
    -> Index dv
    -> State
    -> (Index dv, Bool)

avgRatioT mlt dv s cmd = (s', wrap)
    where
        mlt' = snatToNum mlt - 1
        dv' = maxBound
        boundary = dv' - mlt'
        wrap = s >= boundary
        s' =
            case (cmd, wrap) of
                (Clear, _   ) -> 0
                (_    , True) -> s - boundary
                (Run  , _   ) -> s + mlt' + 1
                (Stop , _   ) -> s

{- Compute the ratio needed to transform one clock period into another. There
 - is no approximation, it is an exact result. This might be inefficient since
 - real-world clocks are not exact in the first place.
 -}
computeParams
    :: Integral a
    => a
       -- ^ Period of clock signal
    -> a
       -- ^ Period of clock scaler output
    -> (a, a)
       -- ^ (multiplier, divider)
computeParams from to = (m, d)
    where
        common = lcm from to
        m      = common `div` to
        d      = common `div` from
