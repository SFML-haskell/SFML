module SFML.Graphics.Image
(
    module SFML.Utils
,   createImage
,   imageFromColor
,   imageFromPixels
,   imageFromFile
,   imageFromMemory
,   imageFromStream
,   copy
,   destroy
,   saveImage
,   imageSize
,   createMaskFromColor
,   copyImage'
,   setPixel
,   getPixel
,   getPixels
,   flipHorizontally
,   flipVertically
)
where


import SFML.Graphics.Color
import SFML.Graphics.Rect
import SFML.Graphics.Types
import SFML.SFCopyable
import SFML.SFException
import SFML.SFResource
import SFML.System.InputStream
import SFML.System.Vector2
import SFML.Utils

import Data.Word (Word8)
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)


checkNull :: Image -> Maybe Image
checkNull img@(Image ptr) = if ptr == nullPtr then Nothing else Just img


-- | Create an image.
--
-- This image is filled with black pixels.
createImage
    :: Int -- ^ Width of the image
    -> Int -- ^ Height of the image
    -> IO (Either SFException Image)

createImage w h =
    let err = SFException "Failed creating image"
    in fmap (tagErr err . checkNull) $ sfImage_create (fromIntegral w) (fromIntegral h)

foreign import ccall unsafe "sfImage_create"
    sfImage_create :: CUInt -> CUInt -> IO Image

--CSFML_GRAPHICS_API sfImage* sfImage_create(unsigned int width, unsigned int height);


-- | Create an image and fill it with a unique color.
imageFromColor
    :: Int   -- ^ Width of the image
    -> Int   -- ^ Height of the image
    -> Color -- ^ Fill color
    -> IO Image

imageFromColor w h color = with color $
    sfImage_createFromColor_helper (fromIntegral w) (fromIntegral h)

foreign import ccall unsafe "sfImage_createFromColor_helper"
    sfImage_createFromColor_helper :: CUInt -> CUInt -> Ptr Color -> IO Image

--CSFML_GRAPHICS_API sfImage* sfImage_createFromColor(unsigned int width, unsigned int height, sfColor color);


-- | Create an image from an array of pixels.
--
-- The pixel array is assumed to contain 32-bits RGBA pixels,
-- and have the given width and height. If not, this is
-- an undefined behaviour.
--
-- If pixels is null, an empty image is created.
imageFromPixels
    :: Int -- ^ Width of the image
    -> Int -- ^ Height of the image
    -> Ptr a -- ^ Array of pixels to copy to the image
    -> IO Image

imageFromPixels w h pixels = sfImage_createFromPixels (fromIntegral w) (fromIntegral h) pixels

foreign import ccall unsafe "sfImage_createFromPixels"
    sfImage_createFromPixels :: CUInt -> CUInt -> Ptr a -> IO Image

--CSFML_GRAPHICS_API sfImage* sfImage_createFromPixels(unsigned int width, unsigned int height, const sfUint8* pixels);


-- | Create an image from a file on disk.

-- The supported image formats are bmp, png, tga, jpg, gif,
-- psd, hdr and pic. Some format options are not supported,
-- like progressive jpeg.
--
-- If this function fails, the image is left unchanged.
imageFromFile :: FilePath -> IO (Maybe Image)

imageFromFile path = fmap checkNull $ withCAString path sfImage_createFromFile

foreign import ccall unsafe "sfImage_createFromFile"
    sfImage_createFromFile :: CString -> IO Image

--CSFML_GRAPHICS_API sfImage* sfImage_createFromFile(const char* filename);


-- | Create an image from a file in memory.
--
-- The supported image formats are bmp, png, tga, jpg, gif,
-- psd, hdr and pic. Some format options are not supported,
-- like progressive jpeg.
--
-- If this function fails, the image is left unchanged.
imageFromMemory
    :: Ptr a -- ^ Pointer to the file data in memory
    -> Int   -- ^ Size of the data to load, in bytes
    -> IO (Maybe Image)

imageFromMemory pixels size = fmap checkNull $ sfImage_createFromMemory pixels (fromIntegral size)

foreign import ccall unsafe "sfImage_createFromMemory"
    sfImage_createFromMemory :: Ptr a -> CUInt -> IO Image

--CSFML_GRAPHICS_API sfImage* sfImage_createFromMemory(const void* data, size_t size);


-- | Create an image from a custom stream.
--
-- The supported image formats are bmp, png, tga, jpg, gif,
-- psd, hdr and pic. Some format options are not supported,
-- like progressive jpeg.
--
-- If this function fails, the image is left unchanged.
imageFromStream :: InputStream -> IO (Maybe Image)

imageFromStream stream = fmap checkNull $ with stream sfImage_createFromStream

foreign import ccall "sfImage_createFromStream"
    sfImage_createFromStream :: Ptr InputStream -> IO Image

--CSFML_GRAPHICS_API sfImage* sfImage_createFromStream(sfInputStream* stream);


instance SFCopyable Image where
    
    {-# INLINABLE copy #-}
    copy = sfImage_copy


foreign import ccall unsafe "sfImage_copy"
    sfImage_copy :: Image -> IO Image

--CSFML_GRAPHICS_API sfImage* sfImage_copy(sfImage* image);


instance SFResource Image where
    
    {-# INLINABLE destroy #-}
    destroy = sfImage_destroy

foreign import ccall unsafe "sfImage_destroy"
    sfImage_destroy :: Image -> IO ()

--CSFML_GRAPHICS_API void sfImage_destroy(sfImage* image);


-- | Save an image to a file on disk.
--
-- The format of the image is automatically deduced from
-- the extension. The supported image formats are bmp, png,
-- tga and jpg. The destination file is overwritten
-- if it already exists. This function fails if the image is empty.
--
-- Return 'True' if saving was successful.
saveImage :: Image -> FilePath -> IO Bool

saveImage image path = fmap (/=0) . withCAString path $ sfImage_saveToFile image

foreign import ccall unsafe "sfImage_saveToFile"
    sfImage_saveToFile :: Image -> CString -> IO CInt

--CSFML_GRAPHICS_API sfBool sfImage_saveToFile(const sfImage* image, const char* filename);


-- | Return the size of an image in pixels.
imageSize :: Image -> IO Vec2u

imageSize image = alloca $ \ptr -> sfImage_getSize_helper image ptr >> peek ptr

foreign import ccall unsafe "sfImage_getSize_helper"
    sfImage_getSize_helper :: Image -> Ptr Vec2u -> IO ()

--CSFML_GRAPHICS_API sfVector2u sfImage_getSize(const sfImage* image);


-- | Create a transparency mask from a specified color-key.
--
-- This function sets the alpha value of every pixel matching
-- the given color to alpha (0 by default), so that they
-- become transparent.
createMaskFromColor
    :: Image -- ^ Image object
    -> Color -- ^ Color to make transparent
    -> Int   -- ^ Alpha value to assign to transparent pixels
    -> IO ()

createMaskFromColor image color alpha = with color $
    \ptrColor -> sfImage_createMaskFromColor_helper image ptrColor (fromIntegral alpha)

foreign import ccall unsafe "sfImage_createMaskFromColor_helper"
    sfImage_createMaskFromColor_helper :: Image -> Ptr Color -> Word8 -> IO ()

--CSFML_GRAPHICS_API void sfImage_createMaskFromColor(sfImage* image, sfColor color, sfUint8 alpha);


-- | Copy pixels from an image onto another
--
-- This function does a slow pixel copy and should not be
-- used intensively. It can be used to prepare a complex
-- static image from several others, but if you need this
-- kind of feature in real-time you'd better use sfRenderTexture.
--
-- If sourceRect is empty, the whole image is copied.
-- If applyAlpha is set to true, the transparency of
-- source pixels is applied. If it is false, the pixels are
-- copied unchanged with their alpha value.
copyImage'
    :: Image   -- ^ Dest image object
    -> Image   -- ^ Source image to copy
    -> Int     -- ^ X coordinate of the destination position
    -> Int     -- ^ Y coordinate of the destination position
    -> IntRect -- ^ Sub-rectangle of the source image to copy
    -> Bool    -- ^ Should the copy take in account the source transparency?
    -> IO ()

copyImage' dst src x y rect transp = with rect $
    \ptrRect ->
        sfImage_copyImage_helper src dst (fromIntegral x) (fromIntegral y) ptrRect (fromIntegral . fromEnum $ transp)

foreign import ccall unsafe "sfImage_copyImage_helper"
    sfImage_copyImage_helper :: Image -> Image -> CUInt -> CUInt -> Ptr IntRect -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfImage_copyImage(sfImage* image, const sfImage* source, unsigned int destX, unsigned int destY, sfIntRect sourceRect, sfBool applyAlpha);


-- | Change the color of a pixel in an image.
--
-- This function doesn't check the validity of the pixel
-- coordinates, using out-of-range values will result in
-- an undefined behaviour.
setPixel
    :: Image -- ^ Image object
    -> Int   -- ^ X coordinate of pixel to change
    -> Int   -- ^ Y coordinate of pixel to change
    -> Color -- ^ New color of the pixel
    -> IO ()

setPixel image x y color = with color $ sfImage_setPixel_helper image (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "sfImage_setPixel_helper"
    sfImage_setPixel_helper :: Image -> CUInt -> CUInt -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API void sfImage_setPixel(sfImage* image, unsigned int x, unsigned int y, sfColor color);


-- | Get the color of a pixel in an image.
--
-- This function doesn't check the validity of the pixel
-- coordinates, using out-of-range values will result in
-- an undefined behaviour.
getPixel
    :: Image -- ^ Image object
    -> Int   -- ^ X coordinate of pixel to get
    -> Int   -- ^ Y coordinate of pixel to get
    -> IO Color

getPixel image x y = alloca $
    \ptr -> sfImage_getPixel_helper image (fromIntegral x) (fromIntegral y) ptr >> peek ptr

foreign import ccall unsafe "sfImage_getPixel_helper"
    sfImage_getPixel_helper :: Image -> CUInt -> CUInt -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API sfColor sfImage_getPixel(const sfImage* image, unsigned int x, unsigned int y);


-- | Get a read-only pointer to the array of pixels of an image.
--
-- The returned value points to an array of RGBA pixels made of
-- 8 bits integers components. The size of the array is
-- getWidth() * getHeight() * 4.
--
-- Warning: the returned pointer may become invalid if you
-- modify the image, so you should never store it for too long.
-- If the image is empty, a null pointer is returned.
getPixels
    :: Image
    -> IO (Ptr a)

getPixels = sfImage_getPixelsPtr

foreign import ccall unsafe "sfImage_getPixelsPtr"
    sfImage_getPixelsPtr :: Image -> IO (Ptr a)

--CSFML_GRAPHICS_API const sfUint8* sfImage_getPixelsPtr(const sfImage* image);


-- | Flip an image horizontally (left <-> right).
flipHorizontally :: Image -> IO ()

flipHorizontally = sfImage_flipHorizontally

foreign import ccall unsafe "sfImage_flipHorizontally"
    sfImage_flipHorizontally :: Image -> IO ()

--CSFML_GRAPHICS_API void sfImage_flipHorizontally(sfImage* image);


-- | Flip an image vertically (top <-> bottom)
flipVertically :: Image -> IO ()

flipVertically = sfImage_flipVertically

foreign import ccall unsafe "sfImage_flipVertically"
    sfImage_flipVertically :: Image -> IO ()

--CSFML_GRAPHICS_API void sfImage_flipVertically(sfImage* image);

