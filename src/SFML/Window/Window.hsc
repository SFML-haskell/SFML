{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Window
(
    module SFML.Window.WindowHandle
,   WindowStyle(..)
,   createWindow
,   windowFromHandle
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
,   requestFocus
,   hasFocus
,   display
,   setFramerateLimit
,   setJoystickThreshold
,   getSystemHandle
,   getMousePosition
,   setMousePosition
)
where


import SFML.SFDisplayable
import SFML.SFResource
import SFML.System.Vector2
import SFML.Window.ContextSettings
import SFML.Window.Event
import SFML.Window.SFWindow
import SFML.Window.Types
import SFML.Window.VideoMode
import SFML.Window.WindowHandle

import Data.Bits ((.|.))
import Data.List (foldl')
import Foreign.C.String (CString, withCAString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable

#include <SFML/Window/Window.h>


data WindowStyle
    = SFNone         -- ^ No border / title bar (this flag and all others are mutually exclusive)
    | SFTitlebar     -- ^ Title bar + fixed border
    | SFResize       -- ^ Titlebar + resizable border + maximize button
    | SFClose        -- ^ Titlebar + close button
    | SFFullscreen   -- ^ Fullscreen mode (this flag and all others are mutually exclusive)
    | SFDefaultStyle -- ^ Default window style
    deriving (Eq, Bounded, Show)


instance Enum WindowStyle where

    fromEnum SFNone         = 0
    fromEnum SFTitlebar     = 1
    fromEnum SFResize       = 2
    fromEnum SFClose        = 4
    fromEnum SFFullscreen   = 8
    fromEnum SFDefaultStyle = 7

    toEnum 0 = SFNone
    toEnum 1 = SFTitlebar
    toEnum 2 = SFResize
    toEnum 4 = SFClose
    toEnum 8 = SFFullscreen
    toEnum 7 = SFDefaultStyle

-- | Construct a new window.
--
-- This function creates the window with the size and pixel
-- depth defined in \a mode. An optional style can be passed to
-- customize the look and behaviour of the window (borders,
-- title bar, resizable, closable, ...). If \a style contains
-- sfFullscreen, then \a mode must be a valid video mode.
--
-- The fourth parameter is a pointer to a structure specifying
-- advanced OpenGL context settings such as antialiasing,
-- depth-buffer bits, etc.

createWindow
    :: VideoMode -- ^ Video mode to use (defines the width, height and depth of the rendering area of the window)
    -> String -- ^ Window title
    -> [WindowStyle] -- ^ Window style
    -> Maybe ContextSettings -- ^ Additional settings for the underlying OpenGL context
    -> IO Window

createWindow vm title styles ctxSettings =
    with vm $ \ptrVM ->
    withCAString title $ \ptrTitle ->
    let style = foldl' (.|.) 0 $ fmap fromEnum styles
    in case ctxSettings of
        Nothing  -> sfWindow_create_helper ptrVM ptrTitle (fromIntegral style) nullPtr
        Just ctx -> with ctx $ sfWindow_create_helper ptrVM ptrTitle (fromIntegral style)

foreign import ccall unsafe "sfWindow_create_helper"
    sfWindow_create_helper :: Ptr VideoMode -> CString -> CUInt -> Ptr ContextSettings -> IO Window

--CSFML_WINDOW_API sfWindow* sfWindow_create(sfVideoMode mode, const char* title, sfUint32 style, const sfContextSettings* settings);


-- | Construct a window from an existing control.
--
-- Use this constructor if you want to create an OpenGL
-- rendering area into an already existing control.
--
-- The second parameter is a pointer to a structure specifying
-- advanced OpenGL context settings such as antialiasing,
-- depth-buffer bits, etc.

windowFromHandle
    :: WindowHandle -- ^ Platform-specific handle of the control
    -> Maybe ContextSettings -- ^ Additional settings for the underlying OpenGL context
    -> IO Window

windowFromHandle hwnd ctxSettings =
    case ctxSettings of
        Nothing  -> sfWindow_createFromHandle hwnd nullPtr
        Just ctx -> with ctx $ sfWindow_createFromHandle hwnd

foreign import ccall unsafe "sfWindow_createFromHandle"
    sfWindow_createFromHandle :: WindowHandle -> Ptr ContextSettings -> IO Window

--CSFML_WINDOW_API sfWindow* sfWindow_createFromHandle(sfWindowHandle handle, const sfContextSettings* settings);


instance SFResource Window where

    {-# INLINABLE destroy #-}
    destroy = sfWindow_destroy

foreign import ccall unsafe "sfWindow_destroy"
    sfWindow_destroy :: Window -> IO ()

--CSFML_WINDOW_API void sfWindow_destroy(sfWindow* window);


instance SFDisplayable Window where

    {-# INLINABLE display #-}
    display = sfWindow_display


instance SFWindow Window where

    {-# INLINABLE close #-}
    close = sfWindow_close

    {-# INLINABLE isWindowOpen #-}
    isWindowOpen wnd = sfWindow_isOpen wnd >>= return . (/=0)

    {-# INLINABLE getWindowSettings #-}
    getWindowSettings wnd =
        alloca $ \ptrCtxSettings -> do
        sfWindow_getSettings_helper wnd ptrCtxSettings
        peek ptrCtxSettings

    {-# INLINABLE pollEvent #-}
    pollEvent wnd =
        alloca $ \ptrEvt -> do
        result <- return . (/=0) =<< sfWindow_pollEvent wnd ptrEvt
        case result of
            True  -> peek ptrEvt >>= return . Just
            False -> return Nothing

    {-# INLINABLE waitEvent #-}
    waitEvent wnd =
        alloca $ \ptr -> do
        result <- sfWindow_waitEvent wnd ptr
        case result of
            0 -> return Nothing
            _ -> peek ptr >>= return . Just

    {-# INLINABLE getWindowPosition #-}
    getWindowPosition wnd = alloca $ \vecPtr -> sfWindow_getPosition_helper wnd vecPtr >> peek vecPtr

    {-# INLINABLE setWindowPosition #-}
    setWindowPosition wnd pos = with pos $ \posPtr -> sfWindow_setPosition_helper wnd posPtr

    {-# INLINABLE getWindowSize #-}
    getWindowSize wnd = alloca $ \vecPtr -> sfWindow_getSize_helper wnd vecPtr >> peek vecPtr

    {-# INLINABLE setWindowSize #-}
    setWindowSize wnd size = with size $ \ptrSize -> sfWindow_setSize_helper wnd ptrSize

    {-# INLINABLE setWindowTitle #-}
    setWindowTitle wnd title = withCAString title $ \ptrTitle -> sfWindow_setTitle wnd ptrTitle

    {-# INLINABLE setWindowIcon #-}
    setWindowIcon = sfWindow_setIcon

    {-# INLINABLE setWindowVisible #-}
    setWindowVisible wnd val = sfWindow_setVisible wnd (fromIntegral . fromEnum $ val)

    {-# INLINABLE setMouseVisible #-}
    setMouseVisible wnd val = sfWindow_setMouseCursorVisible wnd (fromIntegral . fromEnum $ val)

    {-# INLINABLE setVSync #-}
    setVSync wnd val = sfWindow_setVerticalSyncEnabled wnd (fromIntegral . fromEnum $ val)

    {-# INLINABLE setKeyRepeat #-}
    setKeyRepeat wnd val = sfWindow_setKeyRepeatEnabled wnd (fromIntegral . fromEnum $ val)

    {-# INLINABLE setWindowActive #-}
    setWindowActive wnd val = sfWindow_setActive wnd (fromIntegral . fromEnum $ val)

    {-# INLINABLE requestFocus #-}
    requestFocus wnd = sfWindow_requestFocus wnd

    {-# INLINABLE hasFocus #-}
    hasFocus wnd = ((/=0) . fromIntegral) <$> sfWindow_hasFocus wnd

    {-# INLINABLE setFramerateLimit #-}
    setFramerateLimit wnd val = sfWindow_setFramerateLimit wnd (fromIntegral val)

    {-# INLINABLE setJoystickThreshold #-}
    setJoystickThreshold w t = sfWindow_setJoystickThreshold w (realToFrac t)

    {-# INLINABLE getSystemHandle #-}
    getSystemHandle = sfWindow_getSystemHandle

    {-# INLINABLE getMousePosition #-}
    getMousePosition Nothing    = alloca $ \ptr -> sfMouse_getPosition_helper (Window nullPtr) ptr >> peek ptr
    getMousePosition (Just wnd) = alloca $ \ptr -> sfMouse_getPosition_helper wnd ptr >> peek ptr

    {-# INLINABLE setMousePosition #-}
    setMousePosition pos Nothing    = with pos $ \ptr -> sfMouse_setPosition_helper ptr (Window nullPtr)
    setMousePosition pos (Just wnd) = with pos $ \ptr -> sfMouse_setPosition_helper ptr wnd


foreign import ccall unsafe "sfWindow_close"
    sfWindow_close :: Window -> IO ()

--CSFML_WINDOW_API void sfWindow_close(sfWindow* window);

foreign import ccall unsafe "sfWindow_isOpen"
    sfWindow_isOpen :: Window -> IO CChar

--CSFML_WINDOW_API sfBool sfWindow_isOpen(const sfWindow* window);


foreign import ccall unsafe "sfWindow_getSettings_helper"
    sfWindow_getSettings_helper :: Window -> Ptr ContextSettings -> IO ()

--CSFML_WINDOW_API sfContextSettings sfWindow_getSettings(const sfWindow* window);


foreign import ccall unsafe "sfWindow_pollEvent"
    sfWindow_pollEvent :: Window -> Ptr SFEvent -> IO CChar

--CSFML_WINDOW_API sfBool sfWindow_pollEvent(sfWindow* window, sfEvent* event);


foreign import ccall unsafe "sfWindow_waitEvent"
    sfWindow_waitEvent :: Window -> Ptr SFEvent -> IO CInt

--CSFML_WINDOW_API sfBool sfWindow_waitEvent(sfWindow* window, sfEvent* event);


foreign import ccall unsafe "sfWindow_getPosition_helper"
    sfWindow_getPosition_helper :: Window -> Ptr Vec2i -> IO ()

--CSFML_WINDOW_API sfVector2i sfWindow_getPosition(const sfWindow* window);


foreign import ccall unsafe "sfWindow_setPosition_helper"
    sfWindow_setPosition_helper :: Window -> Ptr Vec2i -> IO ()

--CSFML_WINDOW_API void sfWindow_setPosition(sfWindow* window, sfVector2i position);


foreign import ccall unsafe "sfWindow_getSize_helper"
    sfWindow_getSize_helper :: Window -> Ptr Vec2u -> IO ()

--CSFML_WINDOW_API sfVector2u sfWindow_getSize(const sfWindow* window);


foreign import ccall unsafe "sfWindow_setSize_helper"
    sfWindow_setSize_helper :: Window -> Ptr Vec2u -> IO ()

--CSFML_WINDOW_API void sfWindow_setSize(sfWindow* window, sfVector2u size);


foreign import ccall unsafe "sfWindow_setTitle"
    sfWindow_setTitle :: Window -> CString -> IO ()

--CSFML_WINDOW_API void sfWindow_setTitle(sfWindow* window, const char* title);


foreign import ccall unsafe "sfWindow_setIcon"
    sfWindow_setIcon :: Window -> Int -> Int -> Ptr a -> IO ()

--CSFML_WINDOW_API void sfWindow_setIcon(sfWindow* window, unsigned int width, unsigned int height, const sfUint8* pixels);


foreign import ccall unsafe "sfWindow_setVisible"
    sfWindow_setVisible :: Window -> CChar -> IO ()

--CSFML_WINDOW_API void sfWindow_setVisible(sfWindow* window, sfBool visible);


foreign import ccall unsafe "sfWindow_setMouseCursorVisible"
    sfWindow_setMouseCursorVisible :: Window -> CChar -> IO ()

--CSFML_WINDOW_API void sfWindow_setMouseCursorVisible(sfWindow* window, sfBool visible);


foreign import ccall unsafe "sfWindow_setVerticalSyncEnabled"
    sfWindow_setVerticalSyncEnabled :: Window -> CChar -> IO ()

--CSFML_WINDOW_API void sfWindow_setVerticalSyncEnabled(sfWindow* window, sfBool enabled);


foreign import ccall unsafe "sfWindow_setKeyRepeatEnabled"
    sfWindow_setKeyRepeatEnabled :: Window -> CChar -> IO ()

--CSFML_WINDOW_API void sfWindow_setKeyRepeatEnabled(sfWindow* window, sfBool enabled);


foreign import ccall unsafe "sfWindow_setActive"
    sfWindow_setActive :: Window -> CChar -> IO ()

--CSFML_WINDOW_API sfBool sfWindow_setActive(sfWindow* window, sfBool active);


foreign import ccall unsafe "sfWindow_requestFocus"
    sfWindow_requestFocus :: Window -> IO ()

-- CSFML_WINDOW_API void sfWindow_requestFocus(sfWindow* window);


foreign import ccall unsafe "sfWindow_hasFocus"
    sfWindow_hasFocus :: Window -> IO CInt

--CSFML_WINDOW_API sfBool sfWindow_hasFocus(const sfWindow* window);


foreign import ccall unsafe "sfWindow_display"
    sfWindow_display :: Window -> IO ()

--CSFML_WINDOW_API void sfWindow_display(sfWindow* window);


foreign import ccall unsafe "sfWindow_setFramerateLimit"
    sfWindow_setFramerateLimit :: Window -> CUInt -> IO ()

--CSFML_WINDOW_API void sfWindow_setFramerateLimit(sfWindow* window, unsigned int limit);


foreign import ccall unsafe "sfWindow_setJoystickThreshold"
    sfWindow_setJoystickThreshold :: Window -> CFloat -> IO ()

--CSFML_WINDOW_API void sfWindow_setJoystickThreshold(sfWindow* window, float threshold);


foreign import ccall unsafe "sfWindow_getSystemHandle"
    sfWindow_getSystemHandle :: Window -> IO WindowHandle

--CSFML_WINDOW_API sfWindowHandle sfWindow_getSystemHandle(const sfWindow* window);


foreign import ccall unsafe "sfMouse_getPosition_helper"
    sfMouse_getPosition_helper :: Window -> Ptr Vec2i -> IO ()

--CSFML_WINDOW_API sfVector2i sfMouse_getPosition(const sfWindow* relativeTo);


foreign import ccall unsafe "sfMouse_setPosition_helper"
    sfMouse_setPosition_helper :: Ptr Vec2i -> Window -> IO ()

--CSFML_WINDOW_API void sfMouse_setPosition(sfVector2i position, const sfWindow* relativeTo);
