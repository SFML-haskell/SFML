{-# LANGUAGE DeriveDataTypeable #-}
module SFML.Graphics.Text
(
    module SFML.Utils
,   TextException(..)
,   TextStyle(..)
,   createText
,   copy
,   destroy
,   setTextString
,   setTextStringU
,   setTextFont
,   setTextCharacterSize
,   setTextStyle
,   setTextColor
,   getTextString
,   getTextUnicodeString
,   getTextFont
,   getTextCharacterSize
,   getTextStyle
,   getTextColor
,   findTextCharacterPos
,   getTextLocalBounds
,   getTextGlobalBounds
)
where


import SFML.Graphics.Color
import SFML.Graphics.Rect
import SFML.Graphics.Transform
import SFML.Graphics.Transformable
import SFML.Graphics.Types
import SFML.SFCopyable
import SFML.SFResource
import SFML.System.Vector2
import SFML.Utils

import Control.Exception
import Control.Monad
import Data.Typeable
import Data.Bits ((.|.))
import Data.List (foldl')
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable


-- | Text styles.
data TextStyle
    = TextRegular    -- ^ Regular characters, no style
    | TextBold       -- ^ Characters are bold
    | TextItalic     -- ^ Characters are in italic
    | TextUnderlined -- ^ Characters are underlined
    deriving (Eq, Bounded, Show)


instance Enum TextStyle where
    
    fromEnum TextRegular    = 0
    fromEnum TextBold       = 1
    fromEnum TextItalic     = 2
    fromEnum TextUnderlined = 4
    
    toEnum 0 = TextRegular
    toEnum 1 = TextBold
    toEnum 2 = TextItalic
    toEnum 4 = TextUnderlined


data TextException = TextException String deriving (Show, Typeable)

instance Exception TextException


checkNull :: Text -> Maybe Text
checkNull text@(Text ptr) = if ptr == nullPtr then Nothing else Just text


checkNullFont :: Font -> Maybe Font
checkNullFont font@(Font ptr) = if ptr == nullPtr then Nothing else Just font


-- | Create a new text.
createText :: IO (Either TextException Text)
createText =
    let err = TextException "Failed creating text"
    in fmap (tagErr err . checkNull) sfText_create

foreign import ccall unsafe "sfText_create"
    sfText_create :: IO Text

-- \return A new sfText object, or NULL if it failed

--CSFML_GRAPHICS_API sfText* sfText_create(void);


instance SFCopyable Text where
    
    {-# INLINABLE copy #-}
    copy = sfText_copy


foreign import ccall unsafe "sfText_copy"
    sfText_copy :: Text -> IO Text

--CSFML_GRAPHICS_API sfText* sfText_copy(sfText* text);


instance SFResource Text where
    
    {-# INLINABLE destroy #-}
    destroy = sfText_destroy

foreign import ccall unsafe "sfText_destroy"
    sfText_destroy :: Text -> IO ()

--CSFML_GRAPHICS_API void sfText_destroy(sfText* text);


instance Transformable Text where

    {-# INLINABLE setPosition #-}
    setPosition text pos = with pos $ sfText_setPosition_helper text
    
    {-# INLINABLE setRotation #-}
    setRotation t r = sfText_setRotation t (realToFrac r)
    
    {-# INLINABLE setScale #-}
    setScale text s = with s $ sfText_setScale_helper text
    
    {-# INLINABLE setOrigin #-}
    setOrigin text o = with o $ sfText_setOrigin_helper text
    
    {-# INLINABLE getPosition #-}
    getPosition text = alloca $ \ptr -> sfText_getPosition_helper text ptr >> peek ptr
    
    {-# INLINABLE getRotation #-}
    getRotation = sfText_getRotation >=> return . realToFrac
    
    {-# INLINABLE getScale #-}
    getScale text = alloca $ \ptr -> sfText_getScale_helper text ptr >> peek ptr
    
    {-# INLINABLE getOrigin #-}
    getOrigin text = alloca $ \ptr -> sfText_getOrigin_helper text ptr >> peek ptr
    
    {-# INLINABLE move #-}
    move text pos = with pos $ sfText_move_helper text
    
    {-# INLINABLE rotate #-}
    rotate t a = sfText_rotate t (realToFrac a)
    
    {-# INLINABLE scale #-}
    scale text s = with s $ sfText_scale_helper text
    
    {-# INLINABLE getTransform #-}
    getTransform text = alloca $ \ptr -> sfText_getTransform_helper text ptr >> peek ptr
    
    {-# INLINABLE getInverseTransform #-}
    getInverseTransform text = alloca $ \ptr -> sfText_getInverseTransform_helper text ptr >> peek ptr


foreign import ccall unsafe "sfText_setPosition_helper"
    sfText_setPosition_helper :: Text -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfText_setPosition(sfText* text, sfVector2f position);

foreign import ccall unsafe "sfText_setRotation"
    sfText_setRotation :: Text -> CFloat -> IO ()

--CSFML_GRAPHICS_API void sfText_setRotation(sfText* text, float angle);

foreign import ccall unsafe "sfText_setScale_helper"
    sfText_setScale_helper :: Text -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfText_setScale(sfText* text, sfVector2f scale);

foreign import ccall unsafe "sfText_setOrigin_helper"
    sfText_setOrigin_helper :: Text -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfText_setOrigin(sfText* text, sfVector2f origin);

foreign import ccall unsafe "sfText_getPosition_helper"
    sfText_getPosition_helper :: Text -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfText_getPosition(const sfText* text);

foreign import ccall unsafe "sfText_getRotation"
    sfText_getRotation :: Text -> IO CFloat

--CSFML_GRAPHICS_API float sfText_getRotation(const sfText* text);

foreign import ccall unsafe "sfText_getScale_helper"
    sfText_getScale_helper :: Text -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfText_getScale(const sfText* text);

foreign import ccall unsafe "sfText_getOrigin_helper"
    sfText_getOrigin_helper :: Text -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfText_getOrigin(const sfText* text);

foreign import ccall unsafe "sfText_move_helper"
    sfText_move_helper :: Text -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfText_move(sfText* text, sfVector2f offset);

foreign import ccall unsafe "sfText_rotate"
    sfText_rotate :: Text -> CFloat -> IO ()

--CSFML_GRAPHICS_API void sfText_rotate(sfText* text, float angle);

foreign import ccall unsafe "sfText_scale_helper"
    sfText_scale_helper :: Text -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API void sfText_scale(sfText* text, sfVector2f factors);

foreign import ccall unsafe "sfText_getTransform_helper"
    sfText_getTransform_helper :: Text -> Ptr Transform -> IO ()

--CSFML_GRAPHICS_API sfTransform sfText_getTransform(const sfText* text);

foreign import ccall unsafe "sfText_getInverseTransform_helper"
    sfText_getInverseTransform_helper :: Text -> Ptr Transform -> IO ()

--CSFML_GRAPHICS_API sfTransform sfText_getInverseTransform(const sfText* text);


-- | Set the string of a text (from an ANSI string).
--
-- A text's string is empty by default.
setTextString :: Text -> String -> IO ()
setTextString text str = withCAString str $ sfText_setString text

foreign import ccall unsafe "sfText_setString"
    sfText_setString :: Text -> CString -> IO ()

--CSFML_GRAPHICS_API void sfText_setString(sfText* text, const char* string);


-- | Set the string of a text (from a unicode string).
setTextStringU :: Text -> String -> IO ()

-- withCString is locale dependent. SFML expects UTF-32. Needs fix.
setTextStringU text str = withCString str $ sfText_setUnicodeString text

foreign import ccall unsafe "sfText_setUnicodeString"
    sfText_setUnicodeString :: Text -> CString -> IO ()

--CSFML_GRAPHICS_API void sfText_setUnicodeString(sfText* text, const sfUint32* string);


-- | Set the font of a text.
--
-- The font argument refers to a texture that must
-- exist as long as the text uses it. Indeed, the text
-- doesn't store its own copy of the font, but rather keeps
-- a pointer to the one that you passed to this function.
-- If the font is destroyed and the text tries to
-- use it, the behaviour is undefined.
setTextFont :: Text -> Font -> IO ()
setTextFont = sfText_setFont

foreign import ccall unsafe "sfText_setFont"
    sfText_setFont :: Text -> Font -> IO ()

--CSFML_GRAPHICS_API void sfText_setFont(sfText* text, const sfFont* font);


-- | Set the character size of a text.
--
-- The default size is 30.
setTextCharacterSize
    :: Text
    -> Int -- ^ New character size, in pixels
    -> IO ()

setTextCharacterSize text size = sfText_setCharacterSize text (fromIntegral size)

foreign import ccall unsafe "sfText_setCharacterSize"
    sfText_setCharacterSize :: Text -> CUInt -> IO ()

--CSFML_GRAPHICS_API void sfText_setCharacterSize(sfText* text, unsigned int size);


-- | Set the style of a text.
--
-- You can pass a combination of one or more styles, for
-- example [TextBold, sfTextItalic].
--
-- The default style is TextRegular.
setTextStyle :: Text -> [TextStyle] -> IO ()
setTextStyle text styles = sfText_setStyle text $ foldl' (.|.) 0 $ fmap (fromIntegral . fromEnum) styles

-- Fix: Should be using a 32-bit int instead of CUInt.
foreign import ccall unsafe "sfText_setStyle"
    sfText_setStyle :: Text -> CUInt -> IO ()

--CSFML_GRAPHICS_API void sfText_setStyle(sfText* text, sfUint32 style);


-- | Set the global color of a text.
--
-- By default, the text's color is opaque white.
setTextColor :: Text -> Color -> IO ()
setTextColor text color = with color $ sfText_setColor_helper text

foreign import ccall unsafe "sfText_setColor_helper"
    sfText_setColor_helper :: Text -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API void sfText_setColor(sfText* text, sfColor color);


-- | Get the string of a text as an ANSI string.
getTextString :: Text -> IO String
getTextString = sfText_getString >=> peekCString

foreign import ccall unsafe "sfText_getString"
    sfText_getString :: Text -> IO CString

--CSFML_GRAPHICS_API const char* sfText_getString(const sfText* text);


-- | Get the string of a text as a UTF-32 string.
getTextUnicodeString :: Text -> IO String
-- Fix: peekCString is locale-dependent. SFML will return a UTF-32 string.
getTextUnicodeString = sfText_getString >=> peekCString

foreign import ccall unsafe "sfText_getUnicodeString"
    sfText_getUnicodeString :: Text -> IO CString

--CSFML_GRAPHICS_API const sfUint32* sfText_getUnicodeString(const sfText* text);


-- | Get the font used by a text.
--
-- If the text has no font attached, 'Nothing' is returned.
--
-- The returned pointer is const, which means that you can't
-- modify the font when you retrieve it with this function.
getTextFont :: Text -> IO (Maybe Font)
getTextFont = fmap checkNullFont . sfText_getFont

foreign import ccall unsafe "sfText_getFont"
    sfText_getFont :: Text -> IO Font

--CSFML_GRAPHICS_API const sfFont* sfText_getFont(const sfText* text);


-- | Get the size of the characters of a text.
getTextCharacterSize :: Text -> IO Int
getTextCharacterSize = fmap fromIntegral . sfText_getCharacterSize

foreign import ccall unsafe "sfText_getCharacterSize"
    sfText_getCharacterSize :: Text -> IO CUInt

--CSFML_GRAPHICS_API unsigned int sfText_getCharacterSize(const sfText* text);


-- | Get the style of a text
getTextStyle :: Text -> IO TextStyle
getTextStyle = fmap (toEnum . fromIntegral) . sfText_getStyle

foreign import ccall unsafe "sfText_getStyle"
    sfText_getStyle :: Text -> IO CUInt

--CSFML_GRAPHICS_API sfUint32 sfText_getStyle(const sfText* text);


-- | Get the global color of a text.
getTextColor :: Text -> IO Color
getTextColor text = alloca $ \ptr -> sfText_getColor_helper text ptr >> peek ptr

foreign import ccall unsafe "sfText_getColor_helper"
    sfText_getColor_helper :: Text -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API sfColor sfText_getColor(const sfText* text);


-- | Return the position of the ith character in a text.
--
-- This function computes the visual position of a character
-- from its index in the string. The returned position is
-- in global coordinates (translation, rotation, scale and
-- origin are applied).
--
-- If the index is out of range, the position of the end of
-- the string is returned.
findTextCharacterPos
    :: Text
    -> Int -- ^ Index of the character
    -> IO Vec2f

findTextCharacterPos text idx =
    alloca $ \ptr -> sfText_findCharacterPos_helper text (fromIntegral idx) ptr >> peek ptr

foreign import ccall unsafe "sfText_findCharacterPos_helper"
    sfText_findCharacterPos_helper :: Text -> CUInt -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfText_findCharacterPos(const sfText* text, size_t index);


-- | Get the local bounding rectangle of a text.
--
-- The returned rectangle is in local coordinates, which means
-- that it ignores the transformations (translation, rotation,
-- scale, ...) that are applied to the entity.
--
-- In other words, this function returns the bounds of the
-- entity in the entity's coordinate system.
getTextLocalBounds :: Text -> IO FloatRect
getTextLocalBounds text = alloca $ \ptr -> sfText_getLocalBounds_helper text ptr >> peek ptr

foreign import ccall unsafe "sfText_getLocalBounds_helper"
    sfText_getLocalBounds_helper :: Text -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API sfFloatRect sfText_getLocalBounds(const sfText* text);


-- | Get the global bounding rectangle of a text.
--
-- The returned rectangle is in global coordinates, which means
-- that it takes in account the transformations (translation,
-- rotation, scale, ...) that are applied to the entity.
--
-- In other words, this function returns the bounds of the
-- text in the global 2D world's coordinate system.
getTextGlobalBounds :: Text -> IO FloatRect
getTextGlobalBounds text = alloca $ \ptr -> sfText_getGlobalBounds_helper text ptr >> peek ptr

foreign import ccall unsafe "sfText_getGlobalBounds_helper"
    sfText_getGlobalBounds_helper :: Text -> Ptr FloatRect -> IO ()

--CSFML_GRAPHICS_API sfFloatRect sfText_getGlobalBounds(const sfText* text);

