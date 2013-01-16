-- |
-- Module:     Network.Plan9
-- Copyright:  (c) 2013 David M. Rogers
-- License:    GPL3
-- Maintainer: David M. Rogers <predictivestatmech@gmail.com>
--
-- Plan9 is an interface for building plan9 servers (and soon clients)
-- based on the 9P protocol and composed using functional reactive programming.

module Network.Plan9 (
    -- Plan9 re-exports
    module Network.Plan9.Consts
  , module Network.Plan9.Builder
  , module Network.Plan9.Parser
  , module Network.Plan9.Wires
    -- Da Funk
    --Network.Plan9
) where

import Data.Word
import Network.Plan9.Consts
import Network.Plan9.Builder
import Network.Plan9.Parser
import Network.Plan9.Wires

{- $quick_intro

This section is a quick tutorial for the experienced, impatient
Haskell programmer.

This library employs a series of layers in which the
protocol layer gradually gives up its control to the
file-implementation layer.  The termination of this
are the read/write routines.

-}



