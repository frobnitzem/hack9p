hack9p is a pure Haskell implementation of the 9P2000 (Styx) protocol.

  For a general introduction to the 9p protocol and philosophy see:
[cat-v.org](http://man.cat-v.org/plan_9/5/intro).

# What's Included?

  This work spawned out of the NineP hackage library, with some of the
experience I got from playing with Andrew Mirtchovski's
[plan9 implementation in go](http://code.google.com/p/go9p/).

  The basic package 'Network.Plan9.Consts' defines the Qid, Stat and NineMsg
structures.  These (mainly the latter) are used for (de)serializing
(to)from the 9P 'on-wire' protocol representation in Network.Plan9.Builder
and Network.Plan9.Parser.  There's a set of QuickCheck tests
in Tests.hs, which can be run right away with:
> runghc -isrc Tests.hs

These will eventually be developed into full-fledged client and server
examples.

  The core client and server libraries are planned to be build around
netwire's arrows.

# More About Plan 9

  The most up-to-date reference on 9p ideas can be found at: http://9p.cat-v.org

  9P is /the/ network protocol used by the mythical Plan9 operating system.
The big idea is that all resources, networked or local, can be represented as read/write
operations on named hierarchies of objects.  Accessing those should be just as easy as
accessing a file, and it shouldn't make any difference if the file represents a local
or remote resource.  For modern re-inventions of this, see REST API design and FUSE.
Although revolutionary in its scope, the OS was not initially released open
source and required some effort in porting existing software, hindering
widespread adoption.  Its relatively small following led Eric S. Raymond, one of its more vocal
designers, to lament that "the most dangerous enemy of a better solution is an existing
codebase that is just good enough.".
After the decision to leave the personal computer
and Unix networked server markets in 1996, AT&T divested the National Cash Register
Corporation and spun-off Bell Labs as Lucent Technologies, Lucent gave Plan9 a back-seat and
development slowly declined.  In 2002, Lucent made its last (4th edition)
release of the Plan9 OS.  This coincides with the date that Rob Pike moved from
Lucent to Google.  Its major issue is the lack of support for most
hardware.  It spewed some random garbage and ground to a halt during boot on my system.

  In 2011, a fork of the slowly developing v4 was made to allow faster
development, addition of new hardware support, and a re-write of
the fossil filesystem to the "Cached Write Once Read Many Filesystem."
(http://www.sdtimes.com/link/35742)  All I know is that this one did
boot on my (ca. 2002) hardware.  Since it's basically a kernel with some mouse
and keyboard support, I wouldn't recommend it at present unless you're a kernel or
low-level graphics developer.

https://code.google.com/p/plan9front/
  
  For a list of 9P protocol implementations in other languages, see
http://9p.cat-v.org/implementations

