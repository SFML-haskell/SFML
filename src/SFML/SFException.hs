module SFML.SFException where

import Control.Exception

data SFException = SFException String deriving (Show)

instance Exception SFException
