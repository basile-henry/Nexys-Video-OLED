{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Top where

import           Clash.Prelude
import           Data.Char

data Input = Input
  { switches  :: BitVector 8
  , btnC      :: Bit
  , btnL      :: Bit
  , btnU      :: Bit
  , btnR      :: Bit
  , btnD      :: Bit
  , oledReady :: Bool
  }

data Output = Output
  { leds      :: BitVector 8
  , oledChar  :: BitVector 8
  , oledAddr  :: BitVector 8
  , oledStart :: Bool
  }

data State
  = Top
  | Bottom
  deriving (Undefined)

{-# ANN clashTop Synthesize
  { t_name   = "clash_top"
  , t_inputs =
    [ PortProduct ""
      [ PortName "clk"
      , PortName "rst"
      ]
    , PortProduct ""
      [ PortName "switches"
      , PortName "btnC"
      , PortName "btnL"
      , PortName "btnU"
      , PortName "btnR"
      , PortName "btnD"
      , PortName "oledReady"
      ]
    ]
  , t_output =
    PortProduct ""
      [ PortName "leds"
      , PortName "oledChar"
      , PortName "oledAddr"
      , PortName "oledStart"
      ]
  } #-}
clashTop
  :: SystemClockReset
  => Signal System Input
  -> Signal System Output
clashTop = mealy update Bottom
  where
    update state input =
      let (top, bottom) = split $ switches input
          enabled = oledReady input
      in
        case state of
          Bottom ->
            ( if enabled then Top else Bottom
            , output (showHex bottom) 0b000001000 undefined
            )

          Top    ->
            ( if enabled then Bottom else Top
            , output (showHex top) 0b000000000 undefined
            )

    output = Output 4


showHex
  :: BitVector 4
  -> BitVector 8
showHex x = resize x + (fromIntegral . ord $ '0')
