{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Graphics.RenderWindow
(
    createRenderWindow
,   renderWindowFromHandle
,   destroy
,   close
,   isWindowOpen
,   getWindowSettings
,   pollEvent
,   waitEvent
,   getWindowPosition
,   setWindowPosition
,   getWindowSize
,   setWindowSize
,   setWindowTitle
,   setWindowIcon
,   setWindowVisible
,   setMouseVisible
,   setVSync
,   setKeyRepeat
,   setWindowActive
,   display
,   setFramerateLimit
,   setJoystickThreshold
,   getSystemHandle
,   clearRenderWindow
,   setView
,   getView
,   getDefaultView
,   getViewport
,   mapPixelToCoords
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
,   captureRenderWindow
,   getMousePosition
,   setMousePosition
)
where


import SFML.Graphics.Color
import SFML.Graphics.Rect
import SFML.Graphics.Types
import SFML.Graphics.PrimitiveType
import SFML.Graphics.RenderStates
import SFML.Graphics.SFCoordSpace
import SFML.Graphics.SFRenderTarget
import SFML.Graphics.SFViewable
import SFML.Graphics.Vertex
import SFML.Window.ContextSettings
import SFML.Window.Event
import SFML.Window.SFWindow
import SFML.Window.VideoMode
import SFML.Window.WindowHandle
import SFML.Window.Window
import SFML.SFDisplayable
import SFML.SFResource
import SFML.System.Vector2

import Data.Bits ((.|.))
import Data.List (foldl')
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable


-- | Construct a new render window.
createRenderWindow
    :: VideoMode     -- ^ Video mode to use
    -> String        -- ^ Window title
    -> [WindowStyle] -- ^ Window style
    -> Maybe ContextSettings -- ^ Creation settings ('Nothing' to use default values)
    -> IO RenderWindow

createRenderWindow vm title styles ctx =
    withCAString title $ \ctitle ->
    with vm $ \ptrVM ->
    let style = foldl' (.|.) 0 $ fmap fromEnum styles
    in case ctx of
        Nothing -> sfRenderWindow_create_helper ptrVM ctitle (fromIntegral . fromEnum $ style) nullPtr
        Just c  -> with c $ sfRenderWindow_create_helper ptrVM ctitle (fromIntegral . fromEnum $ style)

foreign import ccall unsafe "sfRenderWindow_create_helper"
    sfRenderWindow_create_helper :: Ptr VideoMode -> CString -> CUInt -> Ptr ContextSettings -> IO RenderWindow

--CSFML_GRAPHICS_API sfRenderWindow* sfRenderWindow_create(sfVideoMode mode, const char* title, sfUint32 style, const sfContextSettings* settings);


-- | Construct a render window from an existing control.
renderWindowFromHandle
    :: WindowHandle -- ^ Platform-specific handle of the control
    -> Maybe ContextSettings -- ^ Creation settings ('Nothing' to use default values)
    -> IO RenderWindow

renderWindowFromHandle wm Nothing  = sfRenderWindow_createFromHandle wm nullPtr
renderWindowHandleFrom wm (Just c) = with c $ sfRenderWindow_createFromHandle wm

foreign import ccall unsafe "sfRenderWindow_createFromHandle"
    sfRenderWindow_createFromHandle :: WindowHandle -> Ptr ContextSettings -> IO RenderWindow

--CSFML_GRAPHICS_API sfRenderWindow* sfRenderWindow_createFromHandle(sfWindowHandle handle, const sfContextSettings* settings);


instance SFResource RenderWindow where
    
    {-# INLINABLE destroy #-}
    destroy = sfRenderWindow_destroy

foreign import ccall unsafe "sfRenderWindow_destroy"
    sfRenderWindow_destroy :: RenderWindow -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_destroy(sfRenderWindow* renderWindow);


instance SFDisplayable RenderWindow where
    
    {-# INLINABLE display #-}
    display = sfRenderWindow_display


instance SFWindow RenderWindow where
    
    {-# INLINABLE close #-}    
    close = sfRenderWindow_close
    
    {-# INLINABLE isWindowOpen #-}
    isWindowOpen = fmap (/=0) . sfRenderWindow_isOpen
    
    {-# INLINABLE getWindowSettings #-}
    getWindowSettings wnd = alloca $ \ptr -> sfRenderWindow_getSettings_helper wnd ptr >> peek ptr
    
    {-# INLINABLE pollEvent #-}
    pollEvent wnd =
        alloca $ \ptr -> do
        result <- sfRenderWindow_pollEvent wnd ptr
        case result of
            0 -> return Nothing
            _ -> peek ptr >>= return . Just
    
    {-# INLINABLE waitEvent #-}
    waitEvent wnd =
        alloca $ \ptr -> do
        result <- sfRenderWindow_waitEvent wnd ptr
        case result of
            0 -> return Nothing
            _ -> peek ptr >>= return . Just
    
    {-# INLINABLE getWindowPosition #-}
    getWindowPosition wnd = alloca $ \ptr -> sfRenderWindow_getPosition_helper wnd ptr >> peek ptr
    
    {-# INLINABLE setWindowPosition #-}
    setWindowPosition wnd pos = with pos $ sfRenderWindow_setPosition_helper wnd
    
    {-# INLINABLE getWindowSize #-}
    getWindowSize wnd = alloca $ \ptr -> sfRenderWindow_getSize_helper wnd ptr >> peek ptr
    
    {-# INLINABLE setWindowSize #-}
    setWindowSize wnd size = with size $ sfRenderWindow_setSize_helper wnd
    
    {-# INLINABLE setWindowTitle #-}
    setWindowTitle wnd title = withCAString title $ sfRenderWindow_setTitle wnd
    
    {-# INLINABLE setWindowIcon #-}
    setWindowIcon wnd w h pixels =
        sfRenderWindow_setIcon wnd (fromIntegral w) (fromIntegral h) pixels
    
    {-# INLINABLE setWindowVisible #-}
    setWindowVisible wnd val = sfRenderWindow_setVisible wnd (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setMouseVisible #-}
    setMouseVisible wnd val = sfRenderWindow_setMouseCursorVisible wnd (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setVSync #-}
    setVSync wnd val = sfRenderWindow_setVerticalSyncEnabled wnd (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setKeyRepeat #-}
    setKeyRepeat wnd val = sfRenderWindow_setKeyRepeatEnabled wnd (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setWindowActive #-}
    setWindowActive wnd val =
        fmap (toEnum . fromIntegral) $ sfRenderWindow_setActive wnd (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setFramerateLimit #-}
    setFramerateLimit wnd fps = sfRenderWindow_setFramerateLimit wnd (fromIntegral fps)
    
    {-# INLINABLE setJoystickThreshold #-}
    setJoystickThreshold w t = sfRenderWindow_setJoystickThreshold w (realToFrac t)
    
    {-# INLINABLE getSystemHandle #-}
    getSystemHandle = sfRenderWindow_getSystemHandle
    
    {-# INLINABLE getMousePosition #-}
    getMousePosition Nothing =
        alloca $ \ptr -> sfMouse_getPositionRenderWindow_helper (RenderWindow nullPtr) ptr >> peek ptr
    
    getMousePosition (Just wnd) =
        alloca $ \ptr -> sfMouse_getPositionRenderWindow_helper wnd ptr >> peek ptr
    
    {-# INLINABLE setMousePosition #-}
    setMousePosition pos Nothing =
        with pos $ \ptr -> sfMouse_setPositionRenderWindow_helper ptr (RenderWindow nullPtr)
    
    setMousePosition pos (Just wnd) =
        with pos $ \ptr -> sfMouse_setPositionRenderWindow_helper ptr wnd


foreign import ccall unsafe "sfRenderWindow_close"
    sfRenderWindow_close :: RenderWindow -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_close(sfRenderWindow* renderWindow);

foreign import ccall unsafe "sfRenderWindow_isOpen"
    sfRenderWindow_isOpen :: RenderWindow -> IO CInt

--CSFML_GRAPHICS_API sfBool sfRenderWindow_isOpen(const sfRenderWindow* renderWindow);


foreign import ccall unsafe "sfRenderWindow_getSettings_helper"
    sfRenderWindow_getSettings_helper :: RenderWindow -> Ptr ContextSettings -> IO ()

--CSFML_GRAPHICS_API sfContextSettings sfRenderWindow_getSettings(const sfRenderWindow* renderWindow);


foreign import ccall unsafe "sfRenderWindow_pollEvent"
    sfRenderWindow_pollEvent :: RenderWindow -> Ptr SFEvent -> IO CInt

-- \return sfTrue if an event was returned, sfFalse if event queue was empty

--CSFML_GRAPHICS_API sfBool sfRenderWindow_pollEvent(sfRenderWindow* renderWindow, sfEvent* event);


foreign import ccall unsafe "sfRenderWindow_waitEvent"
    sfRenderWindow_waitEvent :: RenderWindow -> Ptr SFEvent -> IO CInt

-- \return sfFalse if an error occured

--CSFML_GRAPHICS_API sfBool sfRenderWindow_waitEvent(sfRenderWindow* renderWindow, sfEvent* event);


foreign import ccall unsafe "sfRenderWindow_getPosition_helper"
    sfRenderWindow_getPosition_helper :: RenderWindow -> Ptr Vec2i -> IO ()

--CSFML_GRAPHICS_API sfVector2i sfRenderWindow_getPosition(const sfRenderWindow* renderWindow);


foreign import ccall unsafe "sfRenderWindow_setPosition_helper"
    sfRenderWindow_setPosition_helper :: RenderWindow -> Ptr Vec2i -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setPosition(sfRenderWindow* renderWindow, sfVector2i position);


foreign import ccall unsafe "sfRenderWindow_getSize_helper"
    sfRenderWindow_getSize_helper :: RenderWindow -> Ptr Vec2u -> IO ()

--CSFML_GRAPHICS_API sfVector2u sfRenderWindow_getSize(const sfRenderWindow* renderWindow);


foreign import ccall unsafe "sfRenderWindow_setSize_helper"
    sfRenderWindow_setSize_helper :: RenderWindow -> Ptr Vec2u -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setSize(sfRenderWindow* renderWindow, sfVector2u size);


foreign import ccall unsafe "sfRenderWindow_setTitle"
    sfRenderWindow_setTitle :: RenderWindow -> CString -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setTitle(sfRenderWindow* renderWindow, const char* title);


foreign import ccall unsafe "sfRenderWindow_setIcon"
    sfRenderWindow_setIcon :: RenderWindow -> CUInt -> CUInt -> Ptr a -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setIcon(sfRenderWindow* renderWindow, unsigned int width, unsigned int height, const sfUint8* pixels);


foreign import ccall unsafe "sfRenderWindow_setVisible"
    sfRenderWindow_setVisible :: RenderWindow -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setVisible(sfRenderWindow* renderWindow, sfBool visible);


foreign import ccall unsafe "sfRenderWindow_setMouseCursorVisible"
    sfRenderWindow_setMouseCursorVisible :: RenderWindow -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setMouseCursorVisible(sfRenderWindow* renderWindow, sfBool show);


foreign import ccall unsafe "sfRenderWindow_setVerticalSyncEnabled"
    sfRenderWindow_setVerticalSyncEnabled :: RenderWindow -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setVerticalSyncEnabled(sfRenderWindow* renderWindow, sfBool enabled);


foreign import ccall unsafe "sfRenderWindow_setKeyRepeatEnabled"
    sfRenderWindow_setKeyRepeatEnabled :: RenderWindow -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setKeyRepeatEnabled(sfRenderWindow* renderWindow, sfBool enabled);
    

foreign import ccall unsafe "sfRenderWindow_setActive"
    sfRenderWindow_setActive :: RenderWindow -> CInt -> IO CInt

-- \return True if operation was successful, false otherwise

--CSFML_GRAPHICS_API sfBool sfRenderWindow_setActive(sfRenderWindow* renderWindow, sfBool active);


foreign import ccall unsafe "sfRenderWindow_display"
    sfRenderWindow_display :: RenderWindow -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_display(sfRenderWindow* renderWindow);


foreign import ccall unsafe "sfRenderWindow_setFramerateLimit"
    sfRenderWindow_setFramerateLimit :: RenderWindow -> CInt -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setFramerateLimit(sfRenderWindow* renderWindow, unsigned int limit);
    

foreign import ccall unsafe "sfRenderWindow_setJoystickThreshold"
    sfRenderWindow_setJoystickThreshold :: RenderWindow -> CFloat -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setJoystickThreshold(sfRenderWindow* renderWindow, float threshold);


foreign import ccall unsafe "sfRenderWindow_getSystemHandle"
    sfRenderWindow_getSystemHandle :: RenderWindow -> IO WindowHandle

--CSFML_GRAPHICS_API sfWindowHandle sfRenderWindow_getSystemHandle(const sfRenderWindow* renderWindow);


foreign import ccall unsafe "sfMouse_getPositionRenderWindow_helper"
    sfMouse_getPositionRenderWindow_helper :: RenderWindow -> Ptr Vec2i -> IO ()

--CSFML_GRAPHICS_API sfVector2i sfMouse_getPositionRenderWindow(const sfRenderWindow* relativeTo);


foreign import ccall unsafe "sfMouse_setPositionRenderWindow_helper"
    sfMouse_setPositionRenderWindow_helper :: Ptr Vec2i -> RenderWindow -> IO ()

--CSFML_GRAPHICS_API void sfMouse_setPositionRenderWindow(sfVector2i position, const sfRenderWindow* relativeTo);


-- | Clear a render window with the given color.
clearRenderWindow
    :: RenderWindow -- ^ Render window object
    -> Color -- ^ Fill color
    -> IO ()

clearRenderWindow wnd color = with color $ sfRenderWindow_clear_helper wnd

foreign import ccall unsafe "sfRenderWindow_clear_helper"
    sfRenderWindow_clear_helper :: RenderWindow -> Ptr Color -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_clear(sfRenderWindow* renderWindow, sfColor color);


instance SFViewable RenderWindow where
    
    {-# INLINABLE setView #-}
    setView = sfRenderWindow_setView

    {-# INLINABLE getView #-}
    getView = sfRenderWindow_getView
    
    {-# INLINABLE getDefaultView #-}
    getDefaultView = sfRenderWindow_getDefaultView
    
    {-# INLINABLE getViewport #-}
    getViewport wnd view = alloca $ \ptr -> sfRenderWindow_getViewport_helper wnd view ptr >> peek ptr


foreign import ccall unsafe "sfRenderWindow_setView"
    sfRenderWindow_setView :: RenderWindow -> View -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_setView(sfRenderWindow* renderWindow, const sfView* view);

foreign import ccall unsafe "sfRenderWindow_getView"
    sfRenderWindow_getView :: RenderWindow -> IO View

--CSFML_GRAPHICS_API const sfView* sfRenderWindow_getView(const sfRenderWindow* renderWindow);

foreign import ccall unsafe "sfRenderWindow_getDefaultView"
    sfRenderWindow_getDefaultView :: RenderWindow -> IO View

--CSFML_GRAPHICS_API const sfView* sfRenderWindow_getDefaultView(const sfRenderWindow* renderWindow);

foreign import ccall unsafe "sfRenderWindow_getViewport_helper"
    sfRenderWindow_getViewport_helper :: RenderWindow -> View -> Ptr IntRect -> IO ()

--CSFML_GRAPHICS_API sfIntRect sfRenderWindow_getViewport(const sfRenderWindow* renderWindow, const sfView* view);


instance SFCoordSpace RenderWindow where
    
    {-# INLINABLE mapPixelToCoords #-}
    mapPixelToCoords wnd p view =
        alloca $ \ptr ->
        with p $ \posPtr -> case view of
            Nothing -> sfRenderWindow_mapPixelToCoords_helper wnd posPtr (View nullPtr) ptr >> peek ptr
            Just v  -> sfRenderWindow_mapPixelToCoords_helper wnd posPtr v ptr >> peek ptr

foreign import ccall unsafe "sfRenderWindow_mapPixelToCoords_helper"
    sfRenderWindow_mapPixelToCoords_helper :: RenderWindow -> Ptr Vec2i -> View -> Ptr Vec2f -> IO ()

-- \return The converted point, in "world" units

--CSFML_GRAPHICS_API sfVector2f sfRenderWindow_mapPixelToCoords(const sfRenderWindow* renderWindow, sfVector2i point, const sfView* targetView);


instance SFRenderTarget RenderWindow where
    
    {-# INLINABLE drawSprite #-}
    drawSprite wnd sprite Nothing   = sfRenderWindow_drawSprite wnd sprite nullPtr
    drawSprite wnd sprite (Just rs) = with rs $ sfRenderWindow_drawSprite wnd sprite
    
    {-# INLINABLE drawText #-}
    drawText wnd text Nothing   = sfRenderWindow_drawText wnd text nullPtr
    drawText wnd text (Just rs) = with rs $ sfRenderWindow_drawText wnd text
    
    {-# INLINABLE drawShape #-}
    drawShape wnd shape Nothing   = sfRenderWindow_drawShape wnd shape nullPtr
    drawShape wnd shape (Just rs) = with rs $ sfRenderWindow_drawShape wnd shape
    
    {-# INLINABLE drawCircle #-}
    drawCircle wnd circle Nothing   = sfRenderWindow_drawCircleShape wnd circle nullPtr
    drawCircle wnd circle (Just rs) = with rs $ sfRenderWindow_drawCircleShape wnd circle
    
    {-# INLINABLE drawConvexShape #-}
    drawConvexShape wnd shape Nothing   = sfRenderWindow_drawConvexShape wnd shape nullPtr
    drawConvexShape wnd shape (Just rs) = with rs $ sfRenderWindow_drawConvexShape wnd shape
    
    {-# INLINABLE drawRectangle #-}
    drawRectangle wnd rect Nothing   = sfRenderWindow_drawRectangleShape wnd rect nullPtr
    drawRectangle wnd rect (Just rs) = with rs $ sfRenderWindow_drawRectangleShape wnd rect
    
    {-# INLINABLE drawVertexArray #-}
    drawVertexArray wnd va Nothing   = sfRenderWindow_drawVertexArray wnd va nullPtr
    drawVertexArray wnd va (Just rs) = with rs $ sfRenderWindow_drawVertexArray wnd va
    
    {-# INLINABLE drawPrimitives #-}
    drawPrimitives wnd verts prim Nothing =
        let n = length verts
        in withArray verts $ \ptr ->
            sfRenderWindow_drawPrimitives wnd ptr (fromIntegral n) (fromIntegral . fromEnum $ prim) nullPtr
    
    drawPrimitives wnd verts prim (Just r) =
        let n = length verts
        in withArray verts $ \ptr ->
            with r $ sfRenderWindow_drawPrimitives wnd ptr (fromIntegral n) (fromIntegral . fromEnum $ prim)
    
    drawPrimitives' wnd verts n prim Nothing =
        sfRenderWindow_drawPrimitives wnd verts (fromIntegral n) (fromIntegral . fromEnum $ prim) nullPtr
    
    drawPrimitives' wnd verts n prim (Just r) =
        with r $ sfRenderWindow_drawPrimitives wnd verts (fromIntegral n) (fromIntegral . fromEnum $ prim)
    
    {-# INLINABLE pushGLStates #-}
    pushGLStates = sfRenderWindow_pushGLStates
    
    {-# INLINABLE popGLStates #-}
    popGLStates = sfRenderWindow_popGLStates
    
    {-# INLINABLE resetGLStates #-}
    resetGLStates = sfRenderWindow_resetGLStates


foreign import ccall unsafe "sfRenderWindow_drawSprite"
    sfRenderWindow_drawSprite :: RenderWindow -> Sprite -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_drawSprite(sfRenderWindow* renderWindow, const sfSprite* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderWindow_drawText"
    sfRenderWindow_drawText :: RenderWindow -> Text -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_drawText(sfRenderWindow* renderWindow, const sfText* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderWindow_drawShape"
    sfRenderWindow_drawShape :: RenderWindow -> Shape -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_drawShape(sfRenderWindow* renderWindow, const sfShape* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderWindow_drawCircleShape"
    sfRenderWindow_drawCircleShape :: RenderWindow -> CircleShape -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_drawCircleShape(sfRenderWindow* renderWindow, const sfCircleShape* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderWindow_drawConvexShape"
    sfRenderWindow_drawConvexShape :: RenderWindow -> ConvexShape -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_drawConvexShape(sfRenderWindow* renderWindow, const sfConvexShape* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderWindow_drawRectangleShape"
    sfRenderWindow_drawRectangleShape :: RenderWindow -> RectangleShape -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_drawRectangleShape(sfRenderWindow* renderWindow, const sfRectangleShape* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderWindow_drawVertexArray"
    sfRenderWindow_drawVertexArray :: RenderWindow -> VertexArray -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_drawVertexArray(sfRenderWindow* renderWindow, const sfVertexArray* object, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderWindow_drawPrimitives"
    sfRenderWindow_drawPrimitives :: RenderWindow -> Ptr Vertex -> CUInt -> CInt -> Ptr RenderStates -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_drawPrimitives(sfRenderWindow* renderWindow, const sfVertex* vertices, unsigned int vertexCount, sfPrimitiveType type, const sfRenderStates* states);

foreign import ccall unsafe "sfRenderWindow_pushGLStates"
    sfRenderWindow_pushGLStates :: RenderWindow -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_pushGLStates(sfRenderWindow* renderWindow);

foreign import ccall unsafe "sfRenderWindow_popGLStates"
    sfRenderWindow_popGLStates :: RenderWindow -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_popGLStates(sfRenderWindow* renderWindow);

foreign import ccall unsafe "sfRenderWindow_resetGLStates"
    sfRenderWindow_resetGLStates :: RenderWindow -> IO ()

--CSFML_GRAPHICS_API void sfRenderWindow_resetGLStates(sfRenderWindow* renderWindow);


-- | Copy the current contents of a render window to an image.
--
-- This is a slow operation, whose main purpose is to make
-- screenshots of the application. If you want to update an
-- image with the contents of the window and then use it for
-- drawing, you should rather use a 'Texture' and its
-- update(sfWindow*) function.
--
-- You can also draw things directly to a texture with the
-- sfRenderWindow class.
captureRenderWindow :: RenderWindow -> IO Image
captureRenderWindow = sfRenderWindow_capture

foreign import ccall unsafe "sfRenderWindow_capture"
    sfRenderWindow_capture :: RenderWindow -> IO Image

--CSFML_GRAPHICS_API sfImage* sfRenderWindow_capture(const sfRenderWindow* renderWindow);

