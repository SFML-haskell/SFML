{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Window
(
    module SFML.Window.WindowHandle
,   WindowStyle(..)
,   ContextSettings(..)
,   createWindow
,   createWindowFromHandle
,   destroyWindow
,   closeWindow
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
,   setMouseCursorVisible
,   setVerticalSyncEnabled
,   setKeyRepeatEnabled
,   setWindowActive
,   display
,   setFramerateLimit
,   setJoystickThreshold
,   getSystemHandle
)
where


import SFML.Window.Event
import SFML.Window.Types
import SFML.Window.VideoMode
import SFML.System.Vector2
import SFML.Window.WindowHandle

import Data.Bits ((.|.))
import Data.List (foldl1')
import Foreign.C.String (CString, withCAString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable

#include <SFML/Window/Window.h>


sizeInt = #{size int}


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


data ContextSettings = ContextSettings
    { depthBits         :: Int
    , stencilBits       :: Int
    , antialiasingLevel :: Int
    , majorVersion      :: Int
    , minorVersion      :: Int
    }
    deriving (Show)


instance Storable ContextSettings where
    sizeOf _ = 5*sizeInt
    alignment _ = alignment (undefined :: CUInt)
    
    peek ptr = do
        db <- #{peek sfContextSettings, depthBits} ptr
        sb <- #{peek sfContextSettings, stencilBits} ptr
        al <- #{peek sfContextSettings, antialiasingLevel} ptr
        ma <- #{peek sfContextSettings, majorVersion} ptr
        mi <- #{peek sfContextSettings, minorVersion} ptr
        return $ ContextSettings db sb al ma mi
    
    poke ptr (ContextSettings db sb al ma mi) = do
        #{poke sfContextSettings, depthBits} ptr db
        #{poke sfContextSettings, stencilBits} ptr sb
        #{poke sfContextSettings, antialiasingLevel} ptr al
        #{poke sfContextSettings, majorVersion} ptr ma
        #{poke sfContextSettings, minorVersion} ptr mi


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
    -> ContextSettings -- ^ Additional settings for the underlying OpenGL context
    -> IO Window

createWindow vm title styles ctxSettings =
    with vm $ \ptrVM ->
    withCAString title $ \ptrTitle ->
    with ctxSettings $ \ptrCtxSettings ->
    let style = foldl1' (.|.) $ fmap fromEnum styles
    in sfWindow_create_helper ptrVM ptrTitle (fromIntegral style) ptrCtxSettings

foreign import ccall "sfWindow_create_helper"
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

createWindowFromHandle
    :: WindowHandle -- ^ Platform-specific handle of the control
    -> ContextSettings -- ^ Additional settings for the underlying OpenGL context
    -> IO Window

createWindowFromHandle hwnd ctxSettings = do
    with ctxSettings $ \ptrCtxSettings -> sfWindow_createFromHandle hwnd ptrCtxSettings

foreign import ccall "sfWindow_createFromHandle"
    sfWindow_createFromHandle :: WindowHandle -> Ptr ContextSettings -> IO Window

--CSFML_WINDOW_API sfWindow* sfWindow_createFromHandle(sfWindowHandle handle, const sfContextSettings* settings);


-- | Destroy the window.
destroyWindow = sfWindow_destroy

foreign import ccall "sfWindow_destroy"
    sfWindow_destroy :: Window -> IO ()

--CSFML_WINDOW_API void sfWindow_destroy(sfWindow* window);


-- | Close a window and destroy all the attached resources.
--
-- After calling this function, the sfWindow object remains
-- valid, you must call sfWindow_destroy to actually delete it.
--
-- All other functions such as sfWindow_pollEvent or sfWindow_display
-- will still work (i.e. you don't have to test sfWindow_isOpen
-- every time), and will have no effect on closed windows.

closeWindow = sfWindow_close

foreign import ccall "sfWindow_close"
    sfWindow_close :: Window -> IO ()

--CSFML_WINDOW_API void sfWindow_close(sfWindow* window);


-- | Tell whether or not a window is opened
--
-- This function returns whether or not the window exists.
--
-- Note that a hidden window (sfWindow_setVisible(sfFalse)) will return
-- sfTrue.

isWindowOpen :: Window -> IO Bool
isWindowOpen wnd = sfWindow_isOpen wnd >>= return . (/=0)

foreign import ccall "sfWindow_isOpen"
    sfWindow_isOpen :: Window -> IO CChar

--CSFML_WINDOW_API sfBool sfWindow_isOpen(const sfWindow* window);


-- | Get the settings of the OpenGL context of a window.
--
-- Note that these settings may be different from what was
-- passed to the sfWindow_create function,
-- if one or more settings were not supported. In this case,
-- SFML chose the closest match.

getWindowSettings :: Window -> IO ContextSettings
getWindowSettings wnd =
    alloca $ \ptrCtxSettings -> do
    sfWindow_getSettings_helper wnd ptrCtxSettings
    peek ptrCtxSettings

foreign import ccall "sfWindow_getSettings_helper"
    sfWindow_getSettings_helper :: Window -> Ptr ContextSettings -> IO ()

--CSFML_WINDOW_API sfContextSettings sfWindow_getSettings(const sfWindow* window);


-- | Pop the event on top of events stack, if any, and return it.
--
-- This function is not blocking: if there's no pending event then
-- it will return false and leave \a event unmodified.
-- Note that more than one event may be present in the events stack,
-- thus you should always call this function in a loop
-- to make sure that you process every pending event.

pollEvent :: Window -> IO (Maybe SFEvent)
pollEvent wnd =
    alloca $ \ptrEvt -> do
    result <- return . (/=0) =<< sfWindow_pollEvent wnd ptrEvt
    case result of
        True  -> peek ptrEvt >>= return . Just
        False -> return Nothing

foreign import ccall "sfWindow_pollEvent"
    sfWindow_pollEvent :: Window -> Ptr SFEvent -> IO CChar

--CSFML_WINDOW_API sfBool sfWindow_pollEvent(sfWindow* window, sfEvent* event);


-- | Wait for an event and return it.
--
-- This function is blocking: if there's no pending event then
-- it will wait until an event is received.
--
-- After this function returns (and no error occured),
-- the event object is always valid and filled properly.
--
-- This function is typically used when you have a thread that
-- is dedicated to events handling: you want to make this thread
-- sleep as long as no new event is received.

waitEvent :: Window -> IO SFEvent
waitEvent wnd =
    alloca $ \ptrEvt -> do
    sfWindow_waitEvent wnd ptrEvt
    peek ptrEvt

foreign import ccall "sfWindow_waitEvent"
    sfWindow_waitEvent :: Window -> Ptr SFEvent -> IO ()

--CSFML_WINDOW_API sfBool sfWindow_waitEvent(sfWindow* window, sfEvent* event);


-- | Get the position of a window.
getWindowPosition :: Window -> IO Vec2i
getWindowPosition wnd = alloca $ \vecPtr -> sfWindow_getPosition_helper wnd vecPtr >> peek vecPtr

foreign import ccall "sfWindow_getPosition_helper"
    sfWindow_getPosition_helper :: Window -> Ptr Vec2i -> IO ()

--CSFML_WINDOW_API sfVector2i sfWindow_getPosition(const sfWindow* window);


-- | Change the position of a window on screen.
--
-- This function only works for top-level windows
-- (i.e. it will be ignored for windows created from
-- the handle of a child window/control).

setWindowPosition :: Window -> Vec2i -> IO ()
setWindowPosition wnd pos = with pos $ \posPtr -> sfWindow_setPosition_helper wnd posPtr

foreign import ccall "sfWindow_setPosition_helper"
    sfWindow_setPosition_helper :: Window -> Ptr Vec2i -> IO ()

--CSFML_WINDOW_API void sfWindow_setPosition(sfWindow* window, sfVector2i position);


-- | Get the size of the rendering region of a window.
--
-- The size doesn't include the titlebar and borders
-- of the window.

getWindowSize :: Window -> IO Vec2u
getWindowSize wnd = alloca $ \vecPtr -> sfWindow_getSize_helper wnd vecPtr >> peek vecPtr

foreign import ccall "sfWindow_getSize_helper"
    sfWindow_getSize_helper :: Window -> Ptr Vec2u -> IO ()

--CSFML_WINDOW_API sfVector2u sfWindow_getSize(const sfWindow* window);


-- | Change the size of the rendering region of a window.
setWindowSize :: Window -> Vec2u -> IO ()
setWindowSize wnd size = with size $ \ptrSize -> sfWindow_setSize_helper wnd ptrSize

foreign import ccall "sfWindow_setSize_helper"
    sfWindow_setSize_helper :: Window -> Ptr Vec2u -> IO ()

--CSFML_WINDOW_API void sfWindow_setSize(sfWindow* window, sfVector2u size);


-- | Change the title of a window.
setWindowTitle :: Window -> String -> IO ()
setWindowTitle wnd title = withCAString title $ \ptrTitle -> sfWindow_setTitle wnd ptrTitle

foreign import ccall "sfWindow_setTitle"
    sfWindow_setTitle :: Window -> CString -> IO ()

--CSFML_WINDOW_API void sfWindow_setTitle(sfWindow* window, const char* title);


type Pixels = Ptr Int


-- | Change a window's icon.
--
-- Pixels must be an array of \a width x \a height pixels
-- in 32-bits RGBA format.

setWindowIcon
    :: Window
    -> Int -- ^ Icon's width, in pixels
    -> Int -- ^ Icon's height, in pixels
    -> Pixels
    -> IO ()

setWindowIcon = sfWindow_setIcon

foreign import ccall "sfWindow_setIcon"
    sfWindow_setIcon :: Window -> Int -> Int -> Pixels -> IO ()

--CSFML_WINDOW_API void sfWindow_setIcon(sfWindow* window, unsigned int width, unsigned int height, const sfUint8* pixels);


-- | Show or hide a window.
setWindowVisible :: Window -> Bool -> IO ()
setWindowVisible wnd val = sfWindow_setVisible wnd (fromIntegral . fromEnum $ val)

foreign import ccall "sfWindow_setVisible"
    sfWindow_setVisible :: Window -> CChar -> IO ()

--CSFML_WINDOW_API void sfWindow_setVisible(sfWindow* window, sfBool visible);


-- | Show or hide the mouse cursor.
setMouseCursorVisible :: Window -> Bool -> IO ()
setMouseCursorVisible wnd val = sfWindow_setMouseCursorVisible wnd (fromIntegral . fromEnum $ val)

foreign import ccall "sfWindow_setMouseCursorVisible"
    sfWindow_setMouseCursorVisible :: Window -> CChar -> IO ()

--CSFML_WINDOW_API void sfWindow_setMouseCursorVisible(sfWindow* window, sfBool visible);


-- | Enable or disable vertical synchronization.
-- Activating vertical synchronization will limit the number
-- of frames displayed to the refresh rate of the monitor.
--
-- This can avoid some visual artifacts, and limit the framerate
-- to a good value (but not constant across different computers).

setVerticalSyncEnabled :: Window -> Bool -> IO ()
setVerticalSyncEnabled wnd val = sfWindow_setVerticalSyncEnabled wnd (fromIntegral . fromEnum $ val)

foreign import ccall "sfWindow_setVerticalSyncEnabled"
    sfWindow_setVerticalSyncEnabled :: Window -> CChar -> IO ()

--CSFML_WINDOW_API void sfWindow_setVerticalSyncEnabled(sfWindow* window, sfBool enabled);


-- | Enable or disable automatic key-repeat.
--
-- If key repeat is enabled, you will receive repeated
-- KeyPress events while keeping a key pressed. If it is disabled,
-- you will only get a single event when the key is pressed.
--
-- Key repeat is enabled by default.

setKeyRepeatEnabled :: Window -> Bool -> IO ()
setKeyRepeatEnabled wnd val = sfWindow_setKeyRepeatEnabled wnd (fromIntegral . fromEnum $ val)

foreign import ccall "sfWindow_setKeyRepeatEnabled"
    sfWindow_setKeyRepeatEnabled :: Window -> CChar -> IO ()

--CSFML_WINDOW_API void sfWindow_setKeyRepeatEnabled(sfWindow* window, sfBool enabled);


-- | Activate or deactivate a window as the current target for OpenGL rendering.
--
-- A window is active only on the current thread, if you want to
-- make it active on another thread you have to deactivate it
-- on the previous thread first if it was active.
--
-- Only one window can be active on a thread at a time, thus
-- the window previously active (if any) automatically gets deactivated.

setWindowActive :: Window -> Bool -> IO ()
setWindowActive wnd val = sfWindow_setActive wnd (fromIntegral . fromEnum $ val)

foreign import ccall "sfWindow_setActive"
    sfWindow_setActive :: Window -> CChar -> IO ()

--CSFML_WINDOW_API sfBool sfWindow_setActive(sfWindow* window, sfBool active);


-- | Display on screen what has been rendered to the window so far.
--
-- This function is typically called after all OpenGL rendering
-- has been done for the current frame, in order to show
-- it on screen.

display :: Window -> IO ()
display = sfWindow_display

foreign import ccall "sfWindow_display"
    sfWindow_display :: Window -> IO ()

--CSFML_WINDOW_API void sfWindow_display(sfWindow* window);


-- | Limit the framerate to a maximum fixed frequency.
--
-- If a limit is set, the window will use a small delay after
-- each call to sfWindow_display to ensure that the current frame
-- lasted long enough to match the framerate limit.

setFramerateLimit :: Window -> Int -> IO ()
setFramerateLimit wnd val = sfWindow_setFramerateLimit wnd (fromIntegral val)

foreign import ccall "sfWindow_setFramerateLimit"
    sfWindow_setFramerateLimit :: Window -> CUInt -> IO ()

--CSFML_WINDOW_API void sfWindow_setFramerateLimit(sfWindow* window, unsigned int limit);


-- | Change the joystick threshold.
--
-- The joystick threshold is the value below which
-- no JoyMoved event will be generated.

setJoystickThreshold :: Window -> Float -> IO ()
setJoystickThreshold = sfWindow_setJoystickThreshold

foreign import ccall "sfWindow_setJoystickThreshold"
    sfWindow_setJoystickThreshold :: Window -> Float -> IO ()

--CSFML_WINDOW_API void sfWindow_setJoystickThreshold(sfWindow* window, float threshold);


-- | Get the OS-specific handle of the window.
--
-- The type of the returned handle is sfWindowHandle,
-- which is a typedef to the handle type defined by the OS.
--
-- You shouldn't need to use this function, unless you have
-- very specific stuff to implement that SFML doesn't support,
-- or implement a temporary workaround until a bug is fixed.

getSystemHandle :: Window -> WindowHandle
getSystemHandle = sfWindow_getSystemHandle

foreign import ccall "sfWindow_getSystemHandle"
    sfWindow_getSystemHandle :: Window -> WindowHandle

--CSFML_WINDOW_API sfWindowHandle sfWindow_getSystemHandle(const sfWindow* window);

