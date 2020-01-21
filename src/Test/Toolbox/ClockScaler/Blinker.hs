{-
{- Copyright (c) 2017 Peter Lebbing <peter@digitalbrains.com>
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

module Test.Toolbox.ClockScaler.Blinker where

import Clash.Prelude
import Clash.Intel.ClockGen

import Toolbox.ClockScaler
import Toolbox.Misc (snatTH)

-- Flash the LEDs with 2 Hz. LED is a std_logic_vector(7 downto 0) in VHDL,
-- and corresponds to the 8 LEDs on the Altera DE0-Nano.
--
-- This is just a convenient way of checking the basic functionality of the
-- setup.

blinker
    :: HiddenClockResetEnable System
    => Signal System (BitVector 8)
blinker =
        (pack . repeat)
    <$>
        moore xor id False
-- This will work once CλaSH has merged PR #1020
--            (avgPeriod $(snatTH $ hzToPeriod 4) (pure Run))
            (avgRatio d1 $(snatTH $ 100000000 `div` 4)
            (pure Run))

createDomain vSystem{vName="Input", vPeriod=20000}

{-# ANN topEntity
    (Synthesize
        { t_name = "blinker"
        , t_inputs = [ PortName "CLOCK_50"
                     , PortName "KEY0"
                     ]
        , t_output = PortName "LED"
        }) #-}
{-# NOINLINE topEntity #-}
topEntity
    :: Clock Input
    -> Signal Input Bool
    -> Signal System (BitVector 8)
topEntity clkIn rstBtn
    = withClockResetEnable clkSys rstSys enableGen blinker
    where
        (clkSys, sysPllLocked) =
            altpll @System (SSymbol @"altpll100") clkIn
                (unsafeFromLowPolarity rstBtn)
        rstSys =
            resetSynchronizer clkSys (unsafeFromLowPolarity sysPllLocked)
                enableGen
