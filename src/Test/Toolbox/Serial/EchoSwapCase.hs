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

module Test.Toolbox.Serial.EchoSwapCase where

import Clash.Prelude
import Clash.Intel.ClockGen

import qualified Toolbox.ClockScaler as CS
import Toolbox.Serial
import Toolbox.Misc (snatTH)

{- Reads characters from RxD and echoes them on TxD, but bit 5 is inverted.
 - This swaps case on alphabetics
 -}
echoSwapCase
    :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit
echoSwapCase rxd = txd
    where
-- This will work once CλaSH has merged PR #1020
--        tTick = CS.avgPeriod $(snatTH $ hzToPeriod 115200) tScaleCmd
        tTick =
            CS.avgRatio
                $(snatTH $ fst $ CS.computeParams 10000 $ hzToPeriod 115200)
                $(snatTH $ snd $ CS.computeParams 10000 $ hzToPeriod 115200)
                tScaleCmd
        (tScaleCmd, _, txd) = output tTick dValid swappedD
-- This will work once CλaSH has merged PR #1020
--        rTick =
--            CS.avgPeriod $(snatTH $ hzToPeriod $ 16 * 115200) (pure CS.Run)
        rTick =
            CS.avgRatio
                $(snatTH $ fst $ CS.computeParams 10000 $ hzToPeriod
                    $ 16 * 115200)
                $(snatTH $ snd $ CS.computeParams 10000 $ hzToPeriod
                    $ 16 * 115200)
                (pure CS.Run)
        (r, dValid) = input rTick rxd
        (frameErr, dIn) = unbundle r
        swappedD = (xor 32) <$> dIn

createDomain vSystem{vName="Input", vPeriod=20000}

{-# ANN topEntity
    (Synthesize
        { t_name = "serial_test"
        , t_inputs = [ PortName "CLOCK_50"
                     , PortName "KEY0"
                     , PortName "RXD"
                     ]
        , t_output = PortName "TXD"
        }) #-}
{-# NOINLINE topEntity #-}

topEntity
    :: Clock Input
    -> Signal Input Bool
    -> Signal System Bit
    -> Signal System Bit

topEntity clkIn rstBtn
    = withClockResetEnable clkSys rstSys enableGen echoSwapCase
    where
        (clkSys, sysPllLocked) =
            altpll @System (SSymbol @"altpll100") clkIn
                (unsafeFromLowPolarity rstBtn)
        rstSys =
            resetSynchronizer clkSys (unsafeFromLowPolarity sysPllLocked)
                enableGen
