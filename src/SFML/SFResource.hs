module SFML.SFResource
where


class SFResource a where
    
    -- | Destroy the given SFML resource.
    destroy :: a -> IO ()

