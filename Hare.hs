module Hare where

import Data.List (nub)
import Data.Word
import Control.Monad.State
import Test.QuickCheck

import RoverInterface
import RoverModel

-- PART 1: FINDING WAYPOINTS

data Path wp
  = From wp
  | GoTo (Path wp) wp
  deriving (Eq)

instance Show wp => Show (Path wp) where
  show (From x) = "From " ++ show x
  show (GoTo xs x) = show xs ++ " >:> " ++ show x

-- Problem 1. Define a function `wf` that returns `True`
-- precisely if a given `Path` is well-formed according
-- to the rules set out in the specification.

-- Checks:
--    From is always WF
--    Uniqueness of wp's
--    -- x navigable from endpoint of path and path is well-formed
wf :: (Wp wp) => Path wp -> Bool
wf (From _) = True
wf (GoTo xs x) =
  (length $ nub $ pathWps (GoTo xs x)) == (length $ pathWps (GoTo xs x)) && 
  (wfNavigable xs x && wf xs)

-- Returns a list of all wp in a path
pathWps :: (Wp wp) => Path wp -> [wp]
pathWps (From x) = [x]
pathWps (GoTo xs x) = (pathWps xs) ++ [x]

-- Given a path and a wp, ensures the wp is navigable from end of path
wfNavigable :: (Wp wp) => Path wp -> wp -> Bool
wfNavigable p x = x `elem` (navigableFrom $ getWp p)

-- Given a path return the wp value
getWp :: (Wp wp) => Path wp -> wp
getWp (From x) = x
getWp (GoTo _ x) = x

-- Problem 2. Define a smart constructor `>:>` for `GoTo`
-- that returns `Nothing` if adding the given waypoint
-- to the given path would result in a non-well-formed path.
(>:>) :: (Wp wp) => Path wp -> wp -> Maybe (Path wp)
xs >:> x = if wf (GoTo xs x) 
  then Just (GoTo xs x) 
  else Nothing

-- Problem 3. Write a function `extendPath` which returns
-- all possible ways of extending the given `Path` by appending
-- a single additional waypoint.

-- extendPath :: (Wp wp) => Path wp -> [Path wp]
-- extendPath xs = do
--   extendables <- navigableFrom (getWp xs)
--   case xs >:> extendables of
--     Nothing -> pure xs >:> []
--     Just ys -> pure xs >:> ys

extendPath :: (Wp wp) => Path wp -> [Path wp]
extendPath p = map (\x -> (GoTo p x)) $ 
  filter (\y -> wf (GoTo p y)) $ navigableFrom (getWp p)

-- Problem 4. Implement a function `findPaths` which returns
-- all possible ways of extending the given `Path` (by appending
-- any number of additional waypoints) into a path that ends
-- at the given target waypoint.

-- Maybe need to deal with empty case?
findPaths :: (Wp wp) => Path wp -> wp -> [Path wp] -- TODO: THIS FAILS AUTOMARKING
findPaths p t = do
  ex <- extendPath p
  if getWp ex == t
    then pure ex
    else findPaths ex t

-- Efficiency mark 5: your solution should not spend time
-- expanding "useless" partial solutions.




--- PART 2: DISK MATTERS - ENCODE/DECODE

-- The floppy disk drive has no means of tracking the
-- angular position of the spinning magnetic disk.
-- This means that in principle, reading/writing can
-- begin at any position within the track, and the
-- user has no control over where the reading/writing
-- starts from.

-- For example, if you write [1,2,3,4,5,6] on a track of
-- capacity 6, it can happend that reading the track next
-- time will result in [5,6,1,2,3,4] or [2,3,4,5,6,1]. Note
-- however that the disk can spin only in one direction, so
-- you will never get a result like [6,5,4,3,2,1].

-- In this subproblem, you will come up with an encoding
-- scheme gets around the problem of the spinning disk.

-- represents a list of bytes encoded using the scheme

data Encoded = Encoded [Word8] deriving (Show, Eq)

unEncoded :: Encoded -> [Word8]
unEncoded (Encoded xs) = xs

-- Problem 5. Implement a function `rotate` which simulates
-- the effect of the spinning disk by rotating the given
-- list to the left by the given number of entries. E.g.
-- rotate 3 (Encoded [1,2,3,4]) = Encoded [4,1,2,3]
-- Hint: for negative n, you get to choose the behavior.

rotate :: Int -> Encoded -> Encoded
rotate n e = Encoded $ rotate' n (unEncoded e)
  where
    rotate' :: Int -> [Word8] -> [Word8]
    rotate' _ [] = []
    rotate' 0 xs = xs
    rotate' n (w:xs) = rotate' (n - 1) (xs ++ [w])

-- Problem 6. Come up with an encoding scheme which gets
-- around the problem of the spinning disk. More formally,
-- implement a pair of functions, `encode` and `decode`, so
-- that:
--
-- 1. Decoding an encoded list of bytes results in the
-- original list, i.e. decode (encode bs) = Just bs.
-- 2. Decoding is rotationally invariant, i.e.
-- decode . rotate n . encode = Just for any positive n.

-- Encoding scheme: 0 (xs) 0 (xs) 0
encode :: [Word8] -> Encoded
encode xs = Encoded ([0] ++ xs ++ [1] ++ xs ++ [0])

decode :: Encoded -> Maybe [Word8]
decode e = getPattern (balance e)
  -- where
getPattern :: [Word8] -> Maybe [Word8]
getPattern xs = if invalidCode xs ((patternLength xs) + 2)
  then Nothing
  else Just $ take (patternLength xs) (tail xs)

-- Invalid if not balanced after (pattern + 2) rotations
invalidCode :: [Word8] -> Int -> Bool
invalidCode [] _ = True
invalidCode _ 0 = True
invalidCode xs n = if isBalanced xs
  then False
  else invalidCode xs (n - 1)

balance :: Encoded -> [Word8]
balance (Encoded xs) = if isBalanced xs
  then xs
  else balance (rotate 1 (Encoded xs))

isBalanced :: [Word8] -> Bool
isBalanced xs = 
  xs !! 0 == 0 &&
  xs !! ((length xs) - 1) == 0 &&
  xs !! (patternLength xs + 1) == 1 &&
  take (patternLength xs) (tail xs) == drop ((patternLength xs) + 2) (init xs)

patternLength :: [Word8] -> Int
patternLength xs = (length xs - 3) `div` 2

-- Efficiency mark: encoding a list of bytes with length
-- no more than 16 should result in an encoded list of
-- length no more than 37.


-- PART 3: FILE SYSTEM HIERARCHY

-- The rover's in-memory file system is organized into files and
-- directories. Each directory may contain other files and
-- directories inside it. Each file and directory is identified
-- by a unique `Word8`, its UID.


-- You can make the following assumptions about the file
-- system of the rover:
-- 1. The total size of all the files is no more than
--    16kiB (16384 bytes).
-- 2. Every file is at most 3072 bytes long.
-- 3. There are at most 48 files and directories (but their
--    UIDs need not be in the range 0-47) altogether.


-- We have decided that one track on the disk will store the
-- contents of at most one file, i.e. that there will not be
-- any tracks which store multiple files.

-- However, since floppy tracks store only 2048 bytes, and a
-- single file may be longer than 2048 bytes, we will have to
-- come up with a way of storing a single file across multiple
-- tracks.

-- We will divide each file into a list of chunks, so that each
-- chunk is short enough to be stored in a single track. We will
-- assign each chunk its own unique track.

-- To reassemble a file, we have to read and decode each of its
-- chunks from the disk in order, then concatenate the results.

data Chunk =
  Chunk TrackNo Encoded deriving (Show, Eq)

-- Problem 7. Write a stateful function `chunks` which,
-- when given the contents of a file, divides it into
-- a list of `Chunk`s.

-- The state `n` is a `TrackNo` between 0 and 255,
-- denoting the first track that is still available
-- to store data. E.g. if the state is 12, then
-- tracks 0-11 have already been used to store chunks,
-- but tracks 12-39 are still available. If all tracks
-- have been exhausted, signal the error by assiginng
-- the remaining chunks to track 40.

chunks :: [Word8] -> State TrackNo [Chunk]
chunks = error "'chunks' not implemented"

-- The `FSH t` data type represents a file system hierarchy
-- in which each file is annotated with data of type `t`.
-- For example, `FSH [Word8]` can be used to represent the
-- entire file system, where each file is annotated with its
-- contents (a list of bytes), while the type `FSH ()` can
-- be used to represent just the hierarchical relationships
-- between the files and directories (i.e. which contains
-- which), but without any of the file data.

-- Problem 8. Write a lawful Functor instance for the FSH
-- type.

instance Functor FSH where
  fmap = error "'fmap' not implemented for FSH"

-- We will have to save the whole directory hierarchy to
-- disk before the rover is rebooted. So that we can reassemble
-- the hierarchy, we will use Track 0 to store a "header". This
-- header will represent a `FSH [TrackNo]` object, where each
-- file is annotated with the list of tracks that contain its
-- chunks.

-- The `mkHeader` function below will create this header
-- from a file system hierarchy where each file has been
-- annotated with a list of its chunks (assuming your
-- `Functor` instance is correct).

mkHeader :: FSH [Chunk] -> FSH [TrackNo]
mkHeader = fmap (map (\(Chunk n _) -> n))


-- Problem 9. Implement a function `assignTracks` which divides
-- all files in a hierarchy into chunks. Each chunk should have
-- be assigned its unique track number. Do not allocate track 0,
-- as that will be used to store the header.
-- Return `Nothing` if the given file system would not fit on
-- tracks 1-39 of a 40-track disk under your encoding.
-- HINT: You'll probably want to have a separate function
-- with return type `State TrackNo (FSH [Chunk])`.

assignTracks :: FSH [Word8] -> Maybe (FSH [Chunk])
assignTracks = error "'assignTracks' not implemented"


-- PART 4 - DISK CONTROLLER

-- The disk controller supports four operations:
--   headForward - moves the read/write head forward by 1 track.
--   headBackward - moves the r/w head back toward zero by 1 track.
--   readTrack - reads 2048 consecutive bytes from the current track.
--   writeTrack - writes the given list of bytes to the current track.

-- In this problem, you will develop a program `saveFSH` that
-- uses this monad to save the entire file system onto the disk.

-- Problem 10. Write a program `headToTrack` that positions
-- the r/w head of the disk drive on the track with the given
-- number. If the number is larger than 39, position the head
-- on track 39.

headToTrack :: (MonadFloppy m) => Word8 -> m ()
headToTrack = error "'headToTrack' not implemented"

-- Problem 11. Write a program `saveChunk` which writes the
-- given chunk onto the appropriate track of the disk.

saveChunk :: (MonadFloppy m) => Chunk -> m ()
saveChunk = error "'saveChunk' not implemented"


-- The function below calculates the header of the
-- given given `FSH [Chunk]`, and saves it to track 0
-- of the disk. Notice the use of the `toBytes` function.

saveHeader :: (MonadFloppy m) => FSH [Chunk] -> m ()
saveHeader fsh = do
  headToTrack 0
  writeTrack (replicate 2048 0)
  writeTrack (unEncoded $ encode $ toBytes $ mkHeader fsh)


-- Problem 12. Implement a program `saveFSH` that attemps to assign
-- track to the given `fsh` using `assignTracks`. If the assignment
-- was unsuccessful, the program should return False.
-- If the assignment was successful, the program should write the
-- header to track 0 of the disk, then write all the assigned chunks
-- onto the appropriate tracks.

saveFSH :: (MonadFloppy m) => FSH [Word8] -> m Bool
saveFSH = error "'saveFSH' not implemented"

-- Implement a program `loadFSH` that is able to reload a file
-- system from disk. I.e. if `saveFSH fsh` returns `True`, then
-- (saveFSH fsh >> loadFSH) should return `Just fsh`.
-- HINT: To load the header, you might want to use the `fromBytes`
--       function.

loadFSH :: (MonadFloppy m) => m (Maybe (FSH [Word8]))
loadFSH = error "'loadFSH' not implemented"
