module SFML.Graphics.FontInfo
(
    FontInfo(..)
)
where

import Control.Applicative ((<$>), (<*>))
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

#include <SFML/Graphics/FontInfo.h>


data FontInfo
    = FontInfo
    { family :: String
    } deriving (Show)


instance Storable FontInfo where
    sizeOf _ = #{size sfFontInfo}
    alignment _ = #{alignment sfFontInfo}

    peek ptr = FontInfo <$> peekCString (castPtr ptr)

    poke ptr fontInfo = withCString (family fontInfo) $ \cstring -> poke (castPtr ptr) cstring
