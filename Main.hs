{- Minimal example of the findPtr space leak debugging
- technique -}
{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
module Main where

import Control.Concurrent
import System.Environment
import System.Mem.Weak
import System.Mem
import GHC.Exts
import GHC.IO
import GHC.Int
import Numeric

{- Compilation options

1. ghc Main.hs -g3 -debug -fwhole-archive-hs-libs

g3: Add dwarf debugging symbols to the binary
debug: Link using the debug rts so that we can call functions to inspect
the heap
fwhole-archive-libs: Don't pass -gc-sections to the linker so that unused
symbols are retained in libraries.

2. Get the GHC GDB macros - https://gitlab.haskell.org/ghc/ghc/wikis/debugging/compiled-code/.gdbinit

3. Launch `gdb Main`
-}

{- gdb sequence

> run

> Type a string like "abc"; press enter

> Ctrl-C

Now we have paused executation of the process and can inspect the heap
using gdb. Firstly, find where the `A` closures are allocated.

> p (void)findPtr(Main_A_con_info, 1)
(gdb) p (void)findPtr(Main_A_con_info, 1)
0x42001fe9e8 = main:Main.A(0x42001fea42)
-->
0x42001fe0a8 = WEAK(key=0x42001fe9e9 value=0x42001fe9e9 finalizer=0x42001feb10)
0x42001fe0a8 = WEAK(key=0x42001fe9e9 value=0x42001fe9e9 finalizer=0x42001feb10)
0x42001fe700 = THUNK(0x406420, 0x42001fe9e9)

This means that there is an `A` closure allocated at `0x42001fe9e8` which
is pointed to by two other closures.

1. The weak pointer closure
2. A THUNK

We want to know where the thunk comes from, so use the `p4` macro to print
out the THUNK closure.

> p4 0x42001fe700
0x42001fe718:	0x92bcf8 <stg_MUT_ARR_PTRS_CLEAN_info>
0x42001fe710:	0x42001fe9e9
0x42001fe708:	0x0
0x42001fe700:	0x406420 <s3H3_info>

The size of a THUNK closure is three words, confusingly printed in reverse
order. So the last word is a pointer to the info table, the second last is
padding for the indirection pointer and the 2nd word is the payload. The
first word is the start of the next closure.

We can see the info table has dwarf information , so let's see where it is.

> list *0x406420
(gdb) list* 0x406380
0x406380 is in s3H7_info (Main.hs:13).
8	data A = A String deriving Show
9
10	main = do
11	  v <- readLn
12	  let x = A v
13	  let y = id [x,x,x]
14	  wp <- mkWeakPtr x (Just (print "gc"))
15	  performGC
16	  print "start"
17	  xr <- deRefWeak wp

It points directly to line 13 which is where `y` is defined. Now we know
that if we cant GC x until we have gced y. In this case there is no leak as
`y` is referenced later in the program.
-}

data Void

anyToAddrLong :: a -> IO Int64
anyToAddrLong !a = IO (\s -> case anyToAddr# a s of (# s', a' #) -> (# s', I64# (unsafeCoerce# a') #))

weakToAddrLong :: Weak v -> IO (Maybe Int64)
weakToAddrLong w = deRefWeak w >>= traverse anyToAddrLong

data A = A String deriving Show

main = do
  -- Stop `x` being floated for simplicity
  v <- readLn
  -- We want to investigate why `A` closures are being leaked
  -- In this case, `x` is retained by `y`.
  let x = A v
  let y = id [x,x,x]
  -- Make a weak pointer to `x`, when it is GCd then "gc" will be printed.
  -- You can check whether it is still alive by dereferencing it.
  wp <- mkWeakPtr x (Just (print "gc"))
  -- perform a GC before dereferencing the weak pointer so that we know
  -- whether it is dead or not. This also moves the `y` closure from the
  -- nursery so we can find it using `findPtr`.
  performGC
  print "start"
  -- Check if `x` is alive.
  xr <- deRefWeak wp
  print xr
  maddr <- weakToAddrLong wp
  print (fmap (\addr -> "0x" ++ showHex addr "") maddr)
  -- Long pauuse so we can break into `gdb` with Ctrl-C.
  threadDelay 10000000000
  print "end"
  -- Reference to `y` so it stays alive.
  print y
