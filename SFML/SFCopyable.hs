module SFML.SFCopyable
where


class SFCopyable a where
    
    -- | Copy the given SFML resource.
    copy :: a -> IO a

