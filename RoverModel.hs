module RoverModel where

import Data.Word
import RoverInterface

{-
  This is a very simple implementation of `MonadFloppy`,
  mostly useful for testing. Each track is represented
  as a list of bytes `[Word8]`. The state of the disk
  consists of the track under the head (`[Word8]`) and
  two lists `[[Word8]]` representing the tracks behind
  and in front of the head.

  This simple model does not simulate disk spinning: all
  reads and writes start from the same predictable
  location. However, it's still useful for testing and
  debugging your monadic code.

  You are encouraged to develop your own, more sophisticated
  disk model using this one as a template.
-}

data DiskState =
  DiskState [[Word8]] [Word8] [[Word8]]

-- `DiskModel` is very similar to `State DiskState`.
data DiskModel a = DiskModel (DiskState -> (a, DiskState))

-- An empty disk state.
emptyDisk :: DiskState
emptyDisk =
  DiskState [] (replicate 2048 0) (map (\x -> replicate 2048 0) [1..39])

runDiskModel :: DiskModel a -> DiskState -> (a, DiskState)
runDiskModel (DiskModel f) = f

execDiskModel :: DiskModel a -> DiskState -> DiskState
execDiskModel s = snd . runDiskModel s

evalDiskModel :: DiskModel a -> DiskState -> a
evalDiskModel s = fst . runDiskModel s

-- Implemented just like the State monad.
instance Monad DiskModel where
  return x = DiskModel (\s -> (x,s))
  DiskModel f >>= g = DiskModel (\s ->
    let (x,s') = f s in runDiskModel (g x) s')
instance Applicative DiskModel where
  pure = return
  f <*> x = do
    f' <- f
    x' <- x
    return (f' x')
instance Functor DiskModel where
  fmap f x = do
    x' <- x
    return (f x')

-- `DiskModel` implements the four `MonadFloppy`
-- operations in a naive way.
instance MonadFloppy DiskModel where
  headForward = DiskModel $ \(DiskState pre c post) ->
    case post of
      []       -> ((), DiskState pre c post)
      (p:post) -> ((), DiskState (c:pre) p post)
  headBackward = DiskModel $ \(DiskState pre c post) ->
    case pre of
      []      -> ((), DiskState pre c post)
      (p:pre) -> ((), DiskState pre p (c:post))
  readTrack = DiskModel $ \(DiskState pre c post) ->
    (take 2048 (c ++ repeat 0), DiskState pre c post)
  writeTrack t = DiskModel $ \(DiskState pre c post) ->
    ((), DiskState pre (take 2048 $ t ++ drop (length t) c) post)


-- This show function lets us print the first few bytes of
-- the non-zero tracks. Very useful for debugging.
instance Show DiskState where
  show (DiskState pre c post) = "DiskState: \n" ++ concat
    ["  " ++ show n ++ " " ++ p ++ "\n" | (n,p) <- zip [0..] peek] where
      tracks = reverse pre ++ [c] ++ post
      nztracks = filter (/= replicate 2048 0) tracks
      peek = map ((++"..") . show . take 10) nztracks
