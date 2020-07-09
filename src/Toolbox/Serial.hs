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

module Toolbox.Serial where

import Clash.Prelude
--import Control.Applicative
--import GHC.Generics (Generic)
import qualified Toolbox.ClockScaler as CS
import Toolbox.Misc

{-
 - Serial data output routine
 -
 - This routine will send bytes over a serial line according to the
 - asynchronous start/stop protocol. It only supports the dataformat 8N1 (8
 - databits, no parity, 1 stopbit).
 -
 - Inputs:
 -      - ck: This should be the output of a ClockScaler that scales to the
 -        bitclock or baud rate of the serial line. It is True once every
 -        period of the baud rate.
 -      - ld: True if there's a new piece of data on dIn. Should only be
 -        asserted when output "done" is also true.
 -      - dIn: 8-bit word to send out
 -
 - Outputs: (scaler, done, txd):
 -      - scaler: a Toolbox.ClockScaler.State that controls the clock scaler
 -        that does the pacing for this serial output.
 -      - done: True when new data can be accepted because the previous data
 -        has been sent.
 -      - txd: The actual output line where data is sent using asynchronous
 -        start/stop.
 -}

output
    :: HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Unsigned 8)
    -> (Signal dom CS.State, Signal dom Bool, Signal dom Bit)

output ck ld dIn = mealyB output' (repeat 0) (ck, ld, dIn)

output' :: Vec 10 Bit
        -> (Bool, Bool, Unsigned 8)
        -> (Vec 10 Bit, (CS.State, Bool, Bit))

output' shifter (ck, ld, dIn) = (shifter', (scaler, done, txd))
    where
        shifter' = if ld then
                     1 :> (bitCoerce dIn :< 0)
                   else if ck then
                     0 +>> shifter
                   else
                     shifter
        (scaler, done) = if shifter == repeat 0 then
                           (CS.Clear, True )
                         else
                           (CS.Run  , False)
        txd            = if done then
                           1
                         else
                           last shifter

{-
 - A variant of `output` that reads its data from a FIFO
 -
 - If `empty` is asserted, we no longer try to assert `ld`.
 -}

outputFIFO
    :: HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Unsigned 8)
    -> ( Signal dom Bool
       , Signal dom CS.State
       , Signal dom Bit
       )

outputFIFO ck empty dIn = (ld, scaler, txd)
    where
        (scaler, done, txd) = output ck ld dIn
        ld                  = (\(e, d) -> not e && d) <$> bundle (empty, done)

data State = WaitStart | Sample (Unsigned 2) | WaitHigh
    deriving (Show, Generic, NFDataX)

{- Asynchronous start/stop receiver
 -
 - Frame format 8N1 only
 -
 - Inputs:
 -      - cTick: ticks from a continuously running ClockScaler that runs at 16
 -        times the baud rate of the incoming serial line. This divides one
 -        bitperiod in 16 equal parts which are used for the sampling of the
 -        line.
 -      - dIn: the pin of the FPGA the serial data is to be read from.
 -
 - Outputs ((frameErr, dOut), dValid):
 -      - dValid: True if (frameErr, dOut) is valid
 -      - frameErr: True if the stopbit was not "mark" (i.e., it was "space").
 -        This means something went wrong in reception, so the data might be
 -        corrupted as well.
 -      - dOut: The received databyte
 -
 - Majority sampling principle: 3 samples are taken around the center of the
 - databit, a majority counter counts the number of high samples. If this
 - counter is 2 or 3, the most significant bit is high, otherwise, it is low.
 - In other words, the most significant bit is the outcome of the majority
 - vote.
 -}

input
    :: HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom Bit
    -> (Signal dom (Bool, Unsigned 8), Signal dom Bool)

input cTick dIn = mealyB input' initInput (cTick, dInS)
    where
        dInS = register 1 $ register 1 dIn

input'
    :: Mealy (State, Unsigned 4, Bool, Vec 9 Bit, Unsigned 2) (Bool, Bit)
             ((Bool, Unsigned 8), Bool)

input' s@(mode, wait, run, shift, sample) (cTick, dIn)
    = (s', ((frameErr, dOut), dValid))
    where
        s'@(mode', wait', run', shift', sample') = input'' s cTick dIn
        frameErr = head shift' == 0
        dOut = bitCoerce $ tail shift' :: Unsigned 8
        dValid = run && (not run')

input''
    :: (State, Unsigned 4, Bool, Vec 9 Bit, Unsigned 2)
    -> Bool
    -> Bit
    -> (State, Unsigned 4, Bool, Vec 9 Bit, Unsigned 2)

--         mode     wait    run   shift   sample  cTick dIn
input'' s                                         False _
    = s

input'' s@(WaitStart, 0   , _    , shift, sample) _     1
    = s

input''   (WaitStart, 0   , _    , shift, sample) _     0
    = (Sample 1 , 6, False, shift, 0)

input''   (Sample n , 0   , run  , shift, sample) _     dIn
    = case (n, run, last shift) of
       (3, False, _) -> if majority == high then
                          waitEdge -- Not a startbit, but a glitch
                        else
                          (Sample 1, 13, True, 1 :> repeat 0, 0)
       (3, True  , 1) -> waitEdge
       (3, True  , 0) -> (Sample 1, 13, True, shift', 0)
       (_, _     , _) -> (Sample (n + 1), 0, run, shift, sample')
    where
        sample' = if dIn == 1 then
                    sample + 1
                  else
                    sample
        majority = msb sample'
        shift' = majority +>> shift

        waitEdge = if dIn == 1 then
                     (WaitStart, 0, False, shift', sample)
                   else
                     (WaitHigh , 0, False, shift', sample)

input'' s@(WaitHigh , 0   , run  , shift, sample) _     0
    = s

input'' s@(WaitHigh , 0   , run  , shift, sample) _     1
    = (WaitStart, 0, run, shift, sample)

input''   (mode     , wait, run  , shift, sample) _     _
    = (mode, wait - 1, run, shift, sample)

-- Initial state for input
initInput :: (State, Unsigned 4, Bool, Vec 9 Bit, Unsigned 2)

initInput = (WaitStart, 0, False, repeat 0, 0)
