module RoverInterface where

import Data.Word

{-
  The class `Wp` is the class of waypoint types.
  Given a waypoint `x` of type `wp`, the expression
  `navigableFrom x` returns the list of waypoints that
  the rover can reach from waypoint `x`.
-}
class Eq wp => Wp wp where
  navigableFrom :: wp -> [wp]


{-
  TrackNo represents a track number on a floppy disk. Note that
  the disk only has tracks 0-39, although in principle more
  tracks could be supported.
-}
type TrackNo = Word8

{-
  A file or directory unique identifier.
-}
type UID = Word8

{-
  An element of a file system hierarchy.
  E.g. `Dir 0 [File 1 (), Dir 5 [File 2 ()]]`
  represents a file system whose root directory
  contains a file called 1, and another directory
  called 5. Inside 5 there is another file 2.
-}
data FSH a
  = File UID a
  | Dir UID [FSH a]
    deriving (Show, Eq)


{-
  `Monad`s that belong to the `MonadFloppy` class
  let you write programs that control a floppy disk drive.
  Apart from the usual monad operations (bind/return), it
  has four additional operations:
    headForward - moves the r/w head forward by 1 track.
    headBackward - moves the r/w head back toward zero by 1 track.
    readTrack - reads 2048 consecutive bytes from the current track.
    writeTrack - writes the given list of bytes to the current track.
-}
class (Monad m) => (MonadFloppy m) where
  headForward  :: m ()
  headBackward :: m ()
  readTrack :: m [Word8]
  writeTrack :: [Word8] -> m ()


{-
  Types that belong to the Bytes class can be serialized
  into a stream of bytes using `toBytes`, and converted
  back using `fromBytes`. The latter function takes a
  list of bytes, and tries to parse back the prefix of
  this list into something of type `a`. If successful,
  it returns both the parsed value of type `a`, and the
  remaining bytes in the given list. I.e.
  `fromBytes ((toBytes x) ++ ys)`
  should return `(x,ys)`.
-}
class Bytes a where
  toBytes :: a -> [Word8]
  fromBytes :: [Word8] -> Maybe (a, [Word8])

instance Bytes Word8 where
  toBytes x = [x]
  fromBytes [] = Nothing
  fromBytes (x:xs) = Just (x,xs)

instance Bytes a => Bytes [a] where
  toBytes xs = fromIntegral (length xs):concatMap toBytes xs
  fromBytes [] = Nothing
  fromBytes (l:xs) = consume (fromIntegral l) xs where
    consume 0 xs = Just ([],xs)
    consume n xs = do
      (h,xs') <- fromBytes xs
      (t,xs'') <- consume (n-1) xs'
      return ((h:t),xs'')

instance Bytes a => Bytes (FSH a) where
  toBytes (Dir uid cont) =
    0:uid:toBytes cont
  toBytes (File uid ts) = 1:uid:toBytes ts
  fromBytes (0:uid:xs) = do
    (cont,xs') <- fromBytes xs
    return (Dir uid cont, xs')
  fromBytes (1:uid:xs) = do
    (ts,xs') <- fromBytes xs
    return (File uid ts, xs')
  fromBytes _ = Nothing

