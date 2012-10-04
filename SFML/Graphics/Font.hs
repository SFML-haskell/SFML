module SFML.Graphics.Font
(
    fontFromFile
,   fontFromMemory
,   fontFromStream
,   copyFont
,   destroyFont
,   getGlyph
,   getKerning
,   getLineSpacing
,   getTexture
)
where


import SFML.Graphics.Glyph
import SFML.Graphics.Types
import SFML.System.InputStream

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import Unsafe.Coerce


checkNull :: Font -> Maybe Font
checkNull font@(Font ptr) =
    case (unsafeCoerce ptr) of
        0 -> Nothing
        _ -> Just font


-- | Create a new font from a file.
fontFromFile :: FilePath -> IO (Maybe Font)
fontFromFile path = fmap checkNull $ withCAString path sfFont_createFromFile

foreign import ccall unsafe "sfFont_createFromFile"
    sfFont_createFromFile :: CString -> IO Font

--CSFML_GRAPHICS_API sfFont* sfFont_createFromFile(const char* filename);


-- | Create a new image font a file in memory.
fontFromMemory
    :: Ptr Char -- ^ Pointer to the file data in memory
    -> Int -- ^ Size of the data to load, in bytes
    -> IO (Maybe Font)

fontFromMemory pixels size = fmap checkNull $ sfFont_createFromMemory pixels (fromIntegral size)

foreign import ccall unsafe "sfFont_createFromMemory"
    sfFont_createFromMemory :: Ptr a -> CInt -> IO Font

--CSFML_GRAPHICS_API sfFont* sfFont_createFromMemory(const void* data, size_t sizeInBytes);


-- | Create a new image font a custom stream.
fontFromStream :: InputStream -> IO Font
fontFromStream stream = with stream sfFont_createFromStream

foreign import ccall "sfFont_createFromStream"
    sfFont_createFromStream :: Ptr InputStream -> IO Font

--CSFML_GRAPHICS_API sfFont* sfFont_createFromStream(sfInputStream* stream);


-- | Copy an existing font.
copyFont :: Font -> IO Font
copyFont = sfFont_copy

foreign import ccall unsafe "sfFont_copy"
    sfFont_copy :: Font -> IO Font

--CSFML_GRAPHICS_API sfFont* sfFont_copy(sfFont* font);


-- | Destroy an existing font.
destroyFont :: Font -> IO ()
destroyFont = sfFont_destroy

foreign import ccall unsafe "sfFont_destroy"
    sfFont_destroy :: Font -> IO ()

--CSFML_GRAPHICS_API void sfFont_destroy(sfFont* font);


-- | Get a glyph in a font.
getGlyph
    :: Font -- ^ Source font
    -> Int -- ^ Unicode code point of the character to get
    -> Int -- ^ Character size, in pixels
    -> Bool -- ^ Retrieve the bold version or the regular one?
    -> IO Glyph

getGlyph font codePoint size bold =
    alloca $ \glyphPtr -> do
        sfFont_getGlyph_helper
            font (fromIntegral codePoint) (fromIntegral size) (fromIntegral . fromEnum $ bold) glyphPtr
        peek glyphPtr

foreign import ccall unsafe "sfFont_getGlyph_helper"
    sfFont_getGlyph_helper :: Font -> Word32 -> CUInt -> Int -> Ptr Glyph -> IO ()

--CSFML_GRAPHICS_API sfGlyph sfFont_getGlyph(sfFont* font, sfUint32 codePoint, unsigned int characterSize, sfBool bold);


-- | Get the kerning value corresponding to a given pair of characters in a font.
getKerning
    :: Font -- ^ Source font
    -> Int  -- ^ Unicode code point of the first character
    -> Int  -- ^ Unicode code point of the second characte.r
    -> Int  -- ^ Character size, in pixels
    -> IO Int

getKerning font first second size =
    fmap fromIntegral $ sfFont_getKerning font (fromIntegral first) (fromIntegral second) (fromIntegral size)

foreign import ccall unsafe "sfFont_getKerning"
    sfFont_getKerning :: Font -> Word32 -> Word32 -> CUInt -> IO CInt

--CSFML_GRAPHICS_API int sfFont_getKerning(sfFont* font, sfUint32 first, sfUint32 second, unsigned int characterSize);


-- | Get the line spacing value.
getLineSpacing
    :: Font -- ^ Source font
    -> Int    -- ^ Character size, in pixels
    -> IO Int

getLineSpacing font size = fmap fromIntegral $ sfFont_getLineSpacing font (fromIntegral size)

foreign import ccall unsafe "sfFont_getLineSpacing"
    sfFont_getLineSpacing :: Font -> CUInt -> IO CInt

--CSFML_GRAPHICS_API int sfFont_getLineSpacing(sfFont* font, unsigned int characterSize);


-- | Get the texture containing the glyphs of a given size in a font.
getTexture
    :: Font -- ^ Source font
    -> Int    -- ^ Character size, in pixels
    -> IO Texture

getTexture font size = sfFont_getTexture font (fromIntegral size)

foreign import ccall unsafe "sfFont_getTexture"
    sfFont_getTexture :: Font -> CUInt -> IO Texture

--CSFML_GRAPHICS_API const sfTexture* sfFont_getTexture(sfFont* font, unsigned int characterSize);

