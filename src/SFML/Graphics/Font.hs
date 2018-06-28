module SFML.Graphics.Font
(
    module SFML.Utils
,   fontFromFile
,   fontFromMemory
,   fontFromStream
,   copy
,   destroy
,   getGlyph
,   getGlyphWithOutline
,   getKerning
,   getLineSpacing
,   getUnderlinePosition
,   getUnderlineThickness
,   getFontTexture
,   getInfo
)
where


import SFML.Graphics.FontInfo
import SFML.Graphics.Glyph
import SFML.Graphics.Types
import SFML.SFCopyable
import SFML.SFException
import SFML.SFResource
import SFML.System.InputStream
import SFML.Utils

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable


checkNull :: Font -> Maybe Font
checkNull font@(Font ptr) = if ptr == nullPtr then Nothing else Just font


-- | Create a new font from a file.
fontFromFile :: FilePath -> IO (Either SFException Font)
fontFromFile path =
    let err = SFException $ "Failed loading font from file " ++ show path
    in fmap (tagErr err . checkNull) $ withCAString path sfFont_createFromFile

foreign import ccall unsafe "sfFont_createFromFile"
    sfFont_createFromFile :: CString -> IO Font

-- \return A new sfFont object, or NULL if it failed

--CSFML_GRAPHICS_API sfFont* sfFont_createFromFile(const char* filename);


-- | Create a new image font a file in memory.
fontFromMemory
    :: Ptr Char -- ^ Pointer to the file data in memory
    -> Int -- ^ Size of the data to load, in bytes
    -> IO (Either SFException Font)

fontFromMemory pixels size =
    let err = SFException $ "Failed loading font from memory address " ++ show pixels
    in fmap (tagErr err . checkNull) $ sfFont_createFromMemory pixels (fromIntegral size)

foreign import ccall unsafe "sfFont_createFromMemory"
    sfFont_createFromMemory :: Ptr a -> CInt -> IO Font

-- \return A new sfFont object, or NULL if it failed

--CSFML_GRAPHICS_API sfFont* sfFont_createFromMemory(const void* data, size_t sizeInBytes);


-- | Create a new image font a custom stream.
fontFromStream :: InputStream -> IO (Either SFException Font)
fontFromStream stream =
    let err = SFException $ "Failed loading font from stream " ++ show stream
    in fmap (tagErr err . checkNull) $ with stream sfFont_createFromStream

foreign import ccall "sfFont_createFromStream"
    sfFont_createFromStream :: Ptr InputStream -> IO Font

-- \return A new sfFont object, or NULL if it failed

--CSFML_GRAPHICS_API sfFont* sfFont_createFromStream(sfInputStream* stream);


instance SFCopyable Font where

    {-# INLINABLE copy #-}
    copy = sfFont_copy


foreign import ccall unsafe "sfFont_copy"
    sfFont_copy :: Font -> IO Font

--CSFML_GRAPHICS_API sfFont* sfFont_copy(sfFont* font);


instance SFResource Font where

    {-# INLINABLE destroy #-}
    destroy = sfFont_destroy

foreign import ccall unsafe "sfFont_destroy"
    sfFont_destroy :: Font -> IO ()

--CSFML_GRAPHICS_API void sfFont_destroy(sfFont* font);


-- | Get a glyph in a font. Defaults outline to zero.
getGlyph
    :: Font -- ^ Source font
    -> Int -- ^ Unicode code point of the character to get
    -> Int -- ^ Character size, in pixels
    -> Bool -- ^ Retrieve the bold version or the regular one?
    -> IO Glyph

getGlyph font codePoint size bold =
    alloca $ \glyphPtr -> do
        sfFont_getGlyph_helper
            font (fromIntegral codePoint) (fromIntegral size) (fromIntegral . fromEnum $ bold) 0.0 glyphPtr
        peek glyphPtr

-- | Get a glyph in a font with an outline.
getGlyphWithOutline
    :: Font -- ^ Source font
    -> Int -- ^ Unicode code point of the character to get
    -> Int -- ^ Character size, in pixels
    -> Bool -- ^ Retrieve the bold version or the regular one?
    -> Float -- ^ Outline parameter
    -> IO Glyph

getGlyphWithOutline font codePoint size bold outline =
    alloca $ \glyphPtr -> do
        sfFont_getGlyph_helper
            font (fromIntegral codePoint) (fromIntegral size) (fromIntegral . fromEnum $ bold) (realToFrac outline) glyphPtr
        peek glyphPtr

foreign import ccall unsafe "sfFont_getGlyph_helper"
    sfFont_getGlyph_helper :: Font -> Word32 -> CUInt -> CInt -> CFloat -> Ptr Glyph -> IO ()

--CSFML_GRAPHICS_API sfGlyph sfFont_getGlyph(sfFont* font, sfUint32 codePoint, unsigned int characterSize, sfBool bold);


-- | Get the kerning value corresponding to a given pair of characters in a font.
getKerning
    :: Font -- ^ Source font
    -> Int  -- ^ Unicode code point of the first character
    -> Int  -- ^ Unicode code point of the second character
    -> Int  -- ^ Character size, in pixels
    -> IO Float

getKerning font first second size =
    fmap realToFrac $ sfFont_getKerning font (fromIntegral first) (fromIntegral second) (fromIntegral size)

foreign import ccall unsafe "sfFont_getKerning"
    sfFont_getKerning :: Font -> Word32 -> Word32 -> CUInt -> IO CFloat

--CSFML_GRAPHICS_API int sfFont_getKerning(sfFont* font, sfUint32 first, sfUint32 second, unsigned int characterSize);


-- | Get the line spacing value.
getLineSpacing
    :: Font -- ^ Source font
    -> Int  -- ^ Character size, in pixels
    -> IO Float

getLineSpacing font size = fmap realToFrac $ sfFont_getLineSpacing font (fromIntegral size)

foreign import ccall unsafe "sfFont_getLineSpacing"
    sfFont_getLineSpacing :: Font -> CUInt -> IO CFloat

--CSFML_GRAPHICS_API int sfFont_getLineSpacing(sfFont* font, unsigned int characterSize);


-- | Get the position of the underline.
--
-- Underline position is the vertical offset to apply between the
-- baseline and the underline.
getUnderlinePosition
    :: Font -- ^ Source font
    -> Int  -- ^ Reference character size
    -> IO Float

getUnderlinePosition font size
    = fmap realToFrac $ sfFont_getUnderlinePosition font (fromIntegral size)

foreign import ccall unsafe "sfFont_getUnderlinePosition"
    sfFont_getUnderlinePosition :: Font -> CUInt -> IO CFloat

--CSFML_GRAPHICS_API float sfFont_getUnderlinePosition(sfFont* font, unsigned int characterSize);


-- | Get the thickness of the underline.
--
-- Underline thickness is the vertical size of the underline.
getUnderlineThickness
    :: Font -- ^ Source font
    -> Int  -- ^ Reference character size
    -> IO Float

getUnderlineThickness font size
    = fmap realToFrac $ sfFont_getUnderlineThickness font (fromIntegral size)

foreign import ccall unsafe "sfFont_getUnderlineThickness"
    sfFont_getUnderlineThickness :: Font -> CUInt -> IO CFloat

--CSFML_GRAPHICS_API float sfFont_getUnderlineThickness(sfFont* font, unsigned int characterSize);


-- | Get the texture containing the glyphs of a given size in a font.
getFontTexture
    :: Font -- ^ Source font
    -> Int    -- ^ Character size, in pixels
    -> IO Texture

getFontTexture font size = sfFont_getTexture font (fromIntegral size)

foreign import ccall unsafe "sfFont_getTexture"
    sfFont_getTexture :: Font -> CUInt -> IO Texture

--CSFML_GRAPHICS_API const sfTexture* sfFont_getTexture(sfFont* font, unsigned int characterSize);


-- | Get the font information.
--
-- The returned structure will remain valid only if the font
-- is still valid. If the font is invalid an invalid structure
-- is returned.
getInfo :: Font -> IO FontInfo
getInfo font = alloca $ \ptr -> sfFont_getInfo_helper font ptr >> peek ptr

foreign import ccall unsafe "sfFont_getInfo_helper"
    sfFont_getInfo_helper :: Font -> Ptr FontInfo -> IO ()

--CSFML_GRAPHICS_API sfFontInfo sfFont_getInfo(const sfFont* font);
