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
    module Network.Plan9.NineP
  , module Network.Plan9.Parser
    -- Da Funk
    --Network.Plan9
) where

import Data.Word
import Network.Plan9.NineP
import Network.Plan9.Parser

-- | permission bits
dmdir =            0x80000000 :: Word32
dmappend =         0x40000000 :: Word32
dmexcl =           0x20000000 :: Word32
dmmount =          0x10000000 :: Word32
dmauth =           0x08000000 :: Word32
dmtmp =            0x04000000 :: Word32
dmsymlink =        0x02000000 :: Word32
-- 9p2000.u extensions
dmdevice =         0x00800000 :: Word32
dmnamedpipe =      0x00200000 :: Word32
dmsocket =         0x00100000 :: Word32
dmsetuid =         0x00080000 :: Word32
dmsetgid =         0x00040000 :: Word32

-- | qid.types
qtdir =            0x80 :: Word8
qtappend =         0x40 :: Word8
qtexcl =           0x20 :: Word8
qtmount =          0x10 :: Word8
qtauth =           0x08 :: Word8
qttmp =	           0x04 :: Word8
qtlink =           0x02 :: Word8
qtfile =           0x00 :: Word8

{- $quick_intro

This section is a quick tutorial for the experienced, impatient
Haskell programmer.

This library employs a series of layers in which the
protocol layer gradually gives up its control to the
file-implementation layer.  The termination of this
are the read/write routines.

-}



