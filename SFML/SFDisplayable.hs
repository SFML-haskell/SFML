module SFML.SFDisplayable
where


class SFDisplayable a where
    
    -- | Update the target's contents.
    display :: a -> IO ()

