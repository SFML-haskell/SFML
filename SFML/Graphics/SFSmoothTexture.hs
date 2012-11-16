module SFML.Graphics.SFSmoothTexture
where


import SFML.Graphics.Types


class SFSmoothTexture a where
    
    -- | Enable or disable the smooth filter on a texture.
    setSmooth :: a -> Bool -> IO ()
    
    -- | Tell whether the smooth filter is enabled or not for a texture.
    isSmooth :: a -> IO Bool

