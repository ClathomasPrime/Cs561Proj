{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- ^ not needed right now

module Monad
  ( module Monad
  , module Control.Monad.State.Strict
  , module Control.Monad.Writer.Strict
  ) where

import Types
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Monad.Identity
import System.Random
import Data.Array.IO
import Control.Monad

-- type BgpMonad a = WriterT [AS] (StateT NetworkData Identity) a

-- This works, but it's not quite what I want.
-- readerToState :: MonadState s m => ReaderT s m a -> m a
-- readerToState k = get >>= runReaderT k

runStateWriter :: StateT s (Writer w) a -> s -> (a,s,w)
runStateWriter comp = adj . runWriter . runStateT comp
  where adj ((a,s),w) = (a,s,w)

--------------------------------------------------------------------------------

class Monad m => Activator m where
  activate :: [a] -> m [a]

instance Activator Identity where
  activate as = return as

instance Activator IO where
  activate as = shuffle =<< sublist as

instance Activator m => Activator (StateT s m) where
  activate as = lift (activate as)

instance (Monoid w, Activator m) => Activator (WriterT w m) where
  activate as = lift (activate as)

--------------------------------------------------------------------------------

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

sublist :: [a] -> IO [a]
sublist = filterM (const $ randomRIO (False, True))

