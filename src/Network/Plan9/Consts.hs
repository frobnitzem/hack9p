{-# LANGUAGE OverloadedStrings #-}
module Network.Plan9.Consts where

import Data.Word
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L

{-
                  -- * Qid - Server side data type for path tracking (<http://9p.cat-v.org> for details)
                  , Qid(..)
                  , emptyQid
                  -- * Stat - Namespace metadata /(somewhat like a unix fstat)/
                  , Stat(..)
                  , emptyStat

                  -- ** MsgTyp - A message payload type
                  , MsgTyp(..)
                  , typInt

                  -- ** NineMsg - An envelope encapsulating the various 9P2000 messages 
                  , NineMsg(..)
                  , NinePkt
-}                        

-------------- Declare the data types !!! ------------------

-- | A variable message type that encapsulates the valid kinds of messages in a 9P2000 payload
-- see http://man.cat-v.org/plan_9/5/intro
-- and http://ericvh.github.com/9p-rfc/rfc9p2000.u.html
data NineMsg = 
    Tversion { tv_msize :: Word32, -- | An agreed-upon max. message size.
               tv_version :: Text
    }
    | Rversion { rv_msize :: Word32,
                 rv_version :: Text
    }
    | Tauth { tau_afid :: Word32, -- | The auth protocol is obsolete,
              tau_uname :: Text,  -- so we always use NOFID, and "", ""
              tau_aname :: Text
    }
    | Rauth { ra_aqid :: Qid }
    | Rerror { re_ename :: Text
	      -- re_errno :: Word32 -- 9p2000.u extension
    }
    | Tflush { tf_oldtag :: Word16 } -- | Interrupt a pending op.
    | Rflush 
    | Tattach { tat_fid :: Word32, -- | Establish the root fid.
	        tat_afid :: Word32,
		tat_uname :: Text,
		tat_aname :: Text
    }
    | Rattach { rat_qid :: Qid }
    | Twalk { tw_fid :: Word32, -- | Create a fid from a fid.
              tw_newfid :: Word32,
	      tw_wnames :: [Text]
    }
    | Rwalk { rwr_wqid :: [Qid] }
    | Topen { to_fid :: Word32, -- | Start an IO session.
	      to_mode :: Word8
    }
    | Ropen { ro_qid :: Qid,
	      ro_iounit :: Word32
    }
    | Tcreate { tcr_fid :: Word32, -- | Create and start IO.
	        tcr_name :: Text,
		tcr_perm :: Word32,
		tcr_mode :: Word8
    }
    | Rcreate { rcr_qid :: Qid,
	        rcr_iounit :: Word32
    }
    | Tread { trd_fid :: Word32, -- | The quintessential IO op.
	      trd_offset :: Word64,
	      trd_count :: Word32
    }
    | Rread { rrd_dat :: L.ByteString }
    | Twrite { twr_fid :: Word32, -- | And its little brother.
	       twr_offset :: Word64,
	       twr_dat :: L.ByteString
    }
    | Rwrite { rw_count :: Word32 }
    | Tclunk { tcl_fid :: Word32 } -- | This means close, but for any fid.
    | Rclunk
    | Tremove { trm_fid :: Word32 } -- | Sayonara.
    | Rremove
    | Tstat { ts_fid :: Word32 } -- | Read a stat.
    | Rstat { rs_stat :: Stat }
    | Twstat { tws_fid :: Word32, -- | Write a stat.
	       tws_stat :: Stat
    }
    | Rwstat
    deriving (Show, Eq)
-- | The auth protocol is obsolete,
-- so we always use emptyAuth.
emptyAuth = Tauth nofid "" ""
type NinePkt = (Word16,NineMsg)

-- | A type that enumerates all the valid /(and one invalid)/ message type in 9P2000
-- The invalid type would be Terror, but the client initiates all the requests,
-- so sending it doesn't make sense.  We mapped it to TFail here to denote parse
-- errors and the like.
data MsgTyp = TTversion | TRversion | TTauth | TRauth | TTattach | TRattach
    | TFail | TRerror | TTflush | TRflush
    | TTwalk | TRwalk | TTopen | TRopen
    | TTcreate | TRcreate | TTread | TRread | TTwrite | TRwrite
    | TTclunk | TRclunk | TTremove | TRremove | TTstat | TRstat
    | TTwstat | TRwstat
    deriving (Show, Eq, Ord, Enum)

-- | A Plan 9 Qid type.  See http://9p.cat-v.org for more information
data Qid = Qid {
    qid_typ :: Word8, -- qid.type[1] the type of the file (directory, etc.),
                      -- represented as a bit vector corresponding to the high
                      --   8 bits of the file's mode word. 
    qid_vers :: Word32, -- qid.vers[4] version number for given path 
    qid_path :: Word64 -- qid.path[8] the file server's unique identification
		       --   for the file 
} deriving (Eq, Ord, Show)
emptyQid :: Qid
emptyQid = Qid {
    qid_typ = 0,
    qid_vers = 0,
    qid_path = 0
}

-- | Provides information on a path entry at a 9P2000 server
-- reading a directory returns a list of these.
data Stat = Stat {
    st_typ :: Word16, -- type[2] used by the kernel to store the device type (~major #)
    st_dev :: Word32, -- dev[4] used by the kernel to index the particular dev (~minor #)
    st_qid :: Qid, -- @Qid@
    st_mode :: Word32, -- mode[4] permissions and flags 
    st_atime :: Word32, -- atime[4] last access time 
    st_mtime :: Word32, -- mtime[4] last modification time 
    st_length :: Word64, -- length[8] length of file in bytes 
    st_name :: Text, -- name[ s ] file name; must be / if the file is the root directory of the server 
    st_uid :: Text, -- uid[ s ] owner name 
    st_gid :: Text, -- gid[ s ] group name 
    st_muid :: Text -- muid[ s ] name of the user who last modified the file 
-- Extensions:
{-  For dot u extensions:
    There are four new fields in the stat structure supporting 9P2000 extensions - as well as new qid.type bits and mode bits.
    The n_uid, n_gid, and n_muid are numeric hints that clients may use to map
  numeric ids when a string to numeric id mapping facility is not available.
    extension[s] is a string describing special files, depending on the mode
  bit. For DSYMLINK files, the string is the target of the link. For DMDEVICE
  files, the string is "b 1 2" for a block device with major 1, minor 2.
  For normal files, this string is empty.

    st_extension :: Text,-- extension[ s ] For use by the UNIX extension to store data about special files (links, devices, pipes, etc.) 
    st_n_uid :: Word32, -- n_uid[4] numeric id of the user who owns the file 
    st_n_gid :: Word32, -- n_gid[4] numeric id of the group associated with the file 
    st_n_muid :: Word32 -- n_muid[4] numeric id of the user who last modified the file 
-}
} deriving (Eq, Ord, Show)
emptyStat :: Stat
emptyStat = Stat {
    st_typ = 0,
    st_dev = 0,
    st_qid = emptyQid,
    st_mode = 0,
    st_atime = 0,
    st_mtime = 0,
    st_length = 0,
    st_name = "",
    st_uid = "",
    st_gid = "",
    st_muid = ""
 {- .u extensions:
    st_extension = "",
    st_n_uid = 0,
    st_n_gid = 0,
    st_n_muid = 0 -}
}

-- Flags for the mode field in Topen and Tcreate messages
oread   =  0 :: Word8 -- open read-only
owrite  =  1 :: Word8 -- open write-only
ordwr   =  2 :: Word8 -- open read-write
oexec   =  3 :: Word8 -- execute (== read but check execute permission)
otrunc  = 16 :: Word8 -- or'ed in (except for exec), truncate file first
ocexec  = 32 :: Word8 -- or'ed in, close on exec
orclose = 64 :: Word8 -- or'ed in, remove on close
--oexcl   = 0x1000 -- or'ed in, exclusive use (create only)

-- | qid.types (high 8 bits of the st_mode)
qtdir =            0x80 :: Word8
qtappend =         0x40 :: Word8
qtexcl =           0x20 :: Word8
qtmount =          0x10 :: Word8
qtauth =           0x08 :: Word8
qttmp =	           0x04 :: Word8
qtlink =           0x02 :: Word8
qtfile =           0x00 :: Word8

-- | st_mode flags
dmdir =            0x80000000 :: Word32
dmappend =         0x40000000 :: Word32
dmexcl =           0x20000000 :: Word32
dmmount =          0x10000000 :: Word32
dmauth =           0x08000000 :: Word32
dmtmp =            0x04000000 :: Word32
-- | 9p2000.u extensions
dmsymlink =        0x02000000 :: Word32
dmlink   =         0x01000000 :: Word32 -- mode bit for hard link (Unix, 9P2000.u)
dmdevice =         0x00800000 :: Word32
dmnamedpipe =      0x00200000 :: Word32
dmsocket =         0x00100000 :: Word32
dmsetuid =         0x00080000 :: Word32
dmsetgid =         0x00040000 :: Word32
-- | The permission bits
dmread      = 0x4      :: Word8  -- mode bit for read permission
dmwrite     = 0x2      :: Word8  -- mode bit for write permission
dmexec      = 0x1      :: Word8  -- mode bit for execute permission


-- | Special fields specifying blank-ness.
-- In a Wstat these fields won't modify the stat.
notag = 0xFFFF     :: Word16 -- no tag specified
nofid = 0xFFFFFFFF :: Word32 -- no fid specified
nouid = 0xFFFFFFFF :: Word32 -- no uid specified

-- | Some Error Numbers from 9P2000.u
{-
econnreset = syscall.ECONNRESET
eexist     = syscall.EEXIST
einval     = syscall.EINVAL
eio        = syscall.EIO
enotdir    = syscall.ENOTDIR
enoent     = syscall.ENOENT
enosys     = syscall.ENOSYS
eperm      = syscall.EPERM
-}

-- | Some protocol flags:
p9_msize   = 8192 + p9_iohdrsz -- default message size (8192+iohdrsz)
p9_iohdrsz = 24             -- the non-data size of the Twrite messages
p9_port    = 564            -- default port for 9P file servers

-- | Mount flags, if anyone implements a re-arrangeable namespace.
morder  = 0x0003        -- mask for bits defining order of mounting
mrepl   = 0x0000        -- mount replaces object
mbefore = 0x0001        -- mount goes before others in union directory
mafter  = 0x0002        -- mount goes after others in union directory
mcreate = 0x0004        -- permit creation in mounted directory
mcache  = 0x0010        -- cache some data
mmask   = 0x0017        -- all bits on

