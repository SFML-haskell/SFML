{-# LANGUAGE DeriveDataTypeable #-}
module SFML.SFException where

import Control.Exception
import Data.Typeable

data SFException = SFException String deriving (Show, Typeable)

instance Exception SFException
