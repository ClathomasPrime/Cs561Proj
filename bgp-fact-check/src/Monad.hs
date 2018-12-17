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
import Control.Monad.Identity
import Control.Monad.Random
import Data.Random.Extras
import Data.RVar

type BgpMonad a = WriterT [AS] (StateT NetworkData Identity) a

class Monad m => Activator m where
  activate :: [a] -> m [a]

instance Activator Identity where
  activate as = return as

instance Activator IO where
  activate as = sampleRVar (shuffle as)

instance Activator m => Activator (StateT s m) where
  activate as = lift (activate as)

instance (Monoid w, Activator m) => Activator (WriterT w m) where
  activate as = lift (activate as)
