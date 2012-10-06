module SFML.Graphics.SmoothTexture
where


import SFML.Graphics.Types


class SmoothTexture a where
    
    -- | Enable or disable the smooth filter on a texture.
    setSmooth :: a -> Bool -> IO ()
    
    -- | Tell whether the smooth filter is enabled or not for a texture.
    isSmooth :: a -> IO Bool

