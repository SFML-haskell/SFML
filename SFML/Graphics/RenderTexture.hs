module SFML.Graphics.RenderTexture
(
    createRenderTexture
,   destroyRenderTexture
,   getRenderTextureSize
,   setRenderTextureActive
,   displayRenderTexture
,   clearRenderTexture
,   setRenderTextureView
,   getRenderTextureView
,   getRenderTextureDefaultView
,   getRenderTextureViewport
,   convertRenderTextureCoords
,   drawSprite
,   drawText
,   drawShape
,   drawCircle
,   drawConvexShape
,   drawRectangle
,   drawVertexArray
,   drawPrimitives
,   drawPrimitives'
,   pushGLStates
,   popGLStates
,   resetGLStates
,   getTexture
,   setSmooth
,   isSmooth
)
where


import SFML.Graphics.Color
import SFML.Graphics.Rect
import SFML.Graphics.SmoothTexture
import SFML.Graphics.Types
import SFML.Graphics.PrimitiveType
import SFML.Graphics.RenderStates
import SFML.Graphics.RenderTarget
import SFML.Graphics.SmoothTexture
import SFML.Graphics.Vertex
import SFML.System.Vector2

import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable (peek)
import Unsafe.Coerce (unsafeCoerce)


checkNull :: RenderTexture -> Maybe RenderTexture
checkNull tex@(RenderTexture ptr) =
    case (unsafeCoerce ptr) of
        0 -> Nothing
        _ -> Just tex


-- | Construct a new render texture.
createRenderTexture
    :: Int -- ^ Width of the render texture
    -> Int -- ^ Height of the render texture
    -> Bool -- ^ Do you want a depth-buffer attached? (useful only if you're doing 3D OpenGL on the rendertexture)
    -> IO (Maybe RenderTexture)

createRenderTexture w h d =
    fmap checkNull $ sfRenderTexture_create (fromIntegral w) (fromIntegral h) (fromIntegral . fromEnum $ d)

foreign import ccall unsafe "sfRenderTexture_create"
    sfRenderTexture_create :: CUInt -> CUInt -> CInt -> IO RenderTexture

-- \return A new sfRenderTexture object, or NULL if it failed

--CSFML_GRAPHICS_API sfRenderTexture* sfRenderTexture_create(unsigned int width, unsigned int height, sfBool depthBuffer);


-- | Destroy an existing render texture.
destroyRenderTexture :: RenderTexture -> IO ()
destroyRenderTexture = sfRenderTexture_destroy

foreign import ccall unsafe "sfRenderTexture_destroy"
    sfRenderTexture_destroy :: RenderTexture -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_destroy(sfRenderTexture* renderTexture);


-- | Get the size of the rendering region of a render texture.
getRenderTextureSize
    :: RenderTexture
    -> IO Vec2u -- ^ Size in pixels

getRenderTextureSize tex = alloca $ \ptr -> sfRenderTexture_getSize_helper tex ptr >> peek ptr

foreign import ccall unsafe "sfRenderTexture_getSize_helper"
    sfRenderTexture_getSize_helper :: RenderTexture -> Ptr Vec2u -> IO ()

--CSFML_GRAPHICS_API sfVector2u sfRenderTexture_getSize(const sfRenderTexture* renderTexture);


-- | Activate or deactivate a render texture as the current target for rendering.
setRenderTextureActive
    :: RenderTexture -- ^ Render texture object
    -> Bool -- ^ 'True' to activate, 'False' to deactivate
    -> IO Bool -- ^ 'True' if operation was successful, 'False' otherwise

setRenderTextureActive tex val =
    fmap (toEnum . fromIntegral) $ sfRenderTexture_setActive tex (fromIntegral . fromEnum $ val)

foreign import ccall unsafe "sfRenderTexture_setActive"
    sfRenderTexture_setActive :: RenderTexture -> CInt -> IO CInt

--CSFML_GRAPHICS_API sfBool sfRenderTexture_setActive(sfRenderTexture* renderTexture, sfBool active);


-- | Update the contents of the target texture.
displayRenderTexture :: RenderTexture -> IO ()
displayRenderTexture = sfRenderTexture_display

foreign import ccall unsafe "sfRenderTexture_display"
    sfRenderTexture_display :: RenderTexture -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_display(sfRenderTexture* renderTexture);


-- | Clear the rendertexture with the given color.
clearRenderTexture
    :: RenderTexture -- ^ Render texture object
    -> Color -- ^ Fill color
    -> IO ()

clearRenderTexture tex col = with col $ sfRenderTexture_clear_helper tex

foreign import ccall unsafe "sfRenderTexture_clear_helper"
    sfRenderTexture_clear_helper :: RenderTexture -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_clear(sfRenderTexture* renderTexture, sfColor color);


-- | Change the current active view of a render texture.
setRenderTextureView :: RenderTexture -> View -> IO ()
setRenderTextureView = sfRenderTexture_setView

foreign import ccall unsafe "sfRenderTexture_setView"
    sfRenderTexture_setView :: RenderTexture -> View -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_setView(sfRenderTexture* renderTexture, const sfView* view);


-- | Get the current active view of a render texture.
getRenderTextureView :: RenderTexture -> IO View
getRenderTextureView = sfRenderTexture_getView

foreign import ccall unsafe "sfRenderTexture_getView"
    sfRenderTexture_getView :: RenderTexture -> IO View

--CSFML_GRAPHICS_API const sfView* sfRenderTexture_getView(const sfRenderTexture* renderTexture);


-- | Get the default view of a render texture.
getRenderTextureDefaultView :: RenderTexture -> IO View
getRenderTextureDefaultView = sfRenderTexture_getDefaultView

foreign import ccall unsafe "sfRenderTexture_getDefaultView"
    sfRenderTexture_getDefaultView :: RenderTexture -> IO View

--CSFML_GRAPHICS_API const sfView* sfRenderTexture_getDefaultView(const sfRenderTexture* renderTexture);


-- | Get the viewport of a view applied to this target, expressed in pixels in the current target.
getRenderTextureViewport
    :: RenderTexture -- ^ Render texture object
    -> View -- ^ Target view
    -> IO IntRect

getRenderTextureViewport tex view =
    alloca $ \ptr -> sfRenderTexture_getViewport_helper tex view ptr >> peek ptr

foreign import ccall unsafe "sfRenderTexture_getViewport_helper"
    sfRenderTexture_getViewport_helper :: RenderTexture -> View -> Ptr IntRect -> IO ()

--CSFML_GRAPHICS_API sfIntRect sfRenderTexture_getViewport(const sfRenderTexture* renderTexture, const sfView* view);


-- | Convert a point in texture coordinates into view coordinates.
convertRenderTextureCoords
    :: RenderTexture -- ^ Render texture object
    -> Vec2i -- ^ Point to convert, relative to the texture
    -> Maybe View -- ^ Target view to convert the point to ('Nothing' to use the current view)
    -> IO Vec2f

convertRenderTextureCoords tex point view =
    alloca $ \out ->
    with point $ \ptr -> case view of
        Nothing -> sfRenderTexture_convertCoords_helper tex ptr (View nullPtr) out >> peek out
        Just v  -> sfRenderTexture_convertCoords_helper tex ptr v out >> peek out

foreign import ccall unsafe "sfRenderTexture_convertCoords_helper"
    sfRenderTexture_convertCoords_helper :: RenderTexture -> Ptr Vec2i -> View -> Ptr Vec2f -> IO ()

--CSFML_GRAPHICS_API sfVector2f sfRenderTexture_convertCoords(const sfRenderTexture* renderTexture, sfVector2i point, const sfView* targetView);


instance RenderTarget RenderTexture where
    
    drawSprite tex sprite Nothing = sfRenderTexture_drawSprite tex sprite nullPtr
    drawSprite tex sprite (Just r) = with r $ sfRenderTexture_drawSprite tex sprite
    
    drawText tex text Nothing = sfRenderTexture_drawText tex text nullPtr
    drawText tex text (Just r) = with r $ sfRenderTexture_drawText tex text
    
    drawShape tex shape Nothing = sfRenderTexture_drawShape tex shape nullPtr
    drawShape tex shape (Just r) = with r $ sfRenderTexture_drawShape tex shape
    
    drawCircle tex circle Nothing = sfRenderTexture_drawCircleShape tex circle nullPtr
    drawCircle tex circle (Just r) = with r $ sfRenderTexture_drawCircleShape tex circle
    
    drawConvexShape tex shape Nothing = sfRenderTexture_drawConvexShape tex shape nullPtr
    drawConvexShape tex shape (Just r) = with r $ sfRenderTexture_drawConvexShape tex shape
    
    drawRectangle tex shape Nothing = sfRenderTexture_drawRectangleShape tex shape nullPtr
    drawRectangle tex shape (Just r) = with r $ sfRenderTexture_drawRectangleShape tex shape
    
    drawVertexArray tex va Nothing = sfRenderTexture_drawVertexArray tex va nullPtr
    drawVertexArray tex va (Just r) = with r $ sfRenderTexture_drawVertexArray tex va
    
    drawPrimitives tex verts prim Nothing =
        let count = fromIntegral $ length verts
        in withArray verts $ \vertsPtr ->
            sfRenderTexture_drawPrimitives tex vertsPtr count (fromIntegral . fromEnum $ prim) nullPtr
    
    drawPrimitives tex verts prim (Just r) =
        let count = fromIntegral $ length verts
        in withArray verts $ \vertsPtr ->
           with r $ sfRenderTexture_drawPrimitives tex vertsPtr count (fromIntegral . fromEnum $ prim)
    
    drawPrimitives' tex verts count prim Nothing =
        sfRenderTexture_drawPrimitives tex verts (fromIntegral count) (fromIntegral . fromEnum $ prim) nullPtr
    
    drawPrimitives' tex verts count prim (Just r) =
        with r $ sfRenderTexture_drawPrimitives tex verts (fromIntegral count) (fromIntegral . fromEnum $ prim)
    
    pushGLStates = sfRenderTexture_pushGLStates
    
    popGLStates = sfRenderTexture_popGLStates
    
    resetGLStates = sfRenderTexture_resetGLStates


foreign import ccall unsafe "sfRenderTexture_drawSprite"
    sfRenderTexture_drawSprite :: RenderTexture -> Sprite -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_drawSprite(sfRenderTexture* renderTexture, const sfSprite* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderTexture_drawText"
    sfRenderTexture_drawText :: RenderTexture -> Text -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_drawText(sfRenderTexture* renderTexture, const sfText* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderTexture_drawShape"
    sfRenderTexture_drawShape :: RenderTexture -> Shape -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_drawShape(sfRenderTexture* renderTexture, const sfShape* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderTexture_drawCircleShape"
    sfRenderTexture_drawCircleShape :: RenderTexture -> CircleShape -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_drawCircleShape(sfRenderTexture* renderTexture, const sfCircleShape* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderTexture_drawConvexShape"
    sfRenderTexture_drawConvexShape :: RenderTexture -> ConvexShape -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_drawConvexShape(sfRenderTexture* renderTexture, const sfConvexShape* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderTexture_drawRectangleShape"
    sfRenderTexture_drawRectangleShape :: RenderTexture -> RectangleShape -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_drawRectangleShape(sfRenderTexture* renderTexture, const sfRectangleShape* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderTexture_drawVertexArray"
    sfRenderTexture_drawVertexArray :: RenderTexture -> VertexArray -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_drawVertexArray(sfRenderTexture* renderTexture, const sfVertexArray* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderTexture_drawPrimitives"
    sfRenderTexture_drawPrimitives :: RenderTexture -> Ptr Vertex -> CUInt -> CInt -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_drawPrimitives(sfRenderTexture* renderTexture, const sfVertex* vertices, unsigned int vertexCount, sfPrimitiveType type, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderTexture_pushGLStates"
    sfRenderTexture_pushGLStates :: RenderTexture -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_pushGLStates(sfRenderTexture* renderTexture);

foreign import ccall unsafe "sfRenderTexture_popGLStates"
    sfRenderTexture_popGLStates :: RenderTexture -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_popGLStates(sfRenderTexture* renderTexture);

foreign import ccall unsafe "resetGLStates"
    sfRenderTexture_resetGLStates :: RenderTexture -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_resetGLStates(sfRenderTexture* renderTexture);


-- | Get the target texture of a render texture.
getTexture :: RenderTexture -> IO Texture
getTexture = sfRenderTexture_getTexture

foreign import ccall unsafe "sfRenderTexture_getTexture"
    sfRenderTexture_getTexture :: RenderTexture -> IO Texture

--CSFML_GRAPHICS_API const sfTexture* sfRenderTexture_getTexture(const sfRenderTexture* renderTexture);


instance SmoothTexture RenderTexture where
    
    setSmooth tex val = sfRenderTexture_setSmooth tex (fromIntegral . fromEnum $ val)
    
    isSmooth tex = fmap (toEnum . fromIntegral ) $ sfRenderTexture_isSmooth tex

foreign import ccall unsafe "sfRenderTexture_setSmooth"
    sfRenderTexture_setSmooth :: RenderTexture -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfRenderTexture_setSmooth(sfRenderTexture* renderTexture, sfBool smooth);

foreign import ccall unsafe "sfRenderTexture_isSmooth"
    sfRenderTexture_isSmooth :: RenderTexture -> IO CInt

--CSFML_GRAPHICS_API sfBool sfRenderTexture_isSmooth(const sfRenderTexture* renderTexture);

