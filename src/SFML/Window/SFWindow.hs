module SFML.Window.SFWindow
where


import SFML.SFDisplayable
import SFML.SFResource
import SFML.System.Vector2
import SFML.Window.ContextSettings
import SFML.Window.Event
import SFML.Window.Types
import SFML.Window.WindowHandle

import Foreign.Ptr (Ptr)


class (SFResource a, SFDisplayable a) => SFWindow a where
    
    -- | Close the window.
    --
    -- After calling this function, the window object remains
    -- valid; you must call 'destroy' to actually delete it.
    close :: a -> IO ()
    
    -- | Tell whether or not a window is opened
    --
    -- This function returns whether or not the window exists.
    --
    -- Note that a hidden window (setWindowVisible 'False' ) will return
    -- 'True'.
    isWindowOpen :: a -> IO Bool
    
    -- | Get the settings of the OpenGL context of a window.
    --
    -- Note that these settings may be different from what was
    -- passed to the window create function,
    -- if one or more settings were not supported. In this case,
    -- SFML chose the closest match.
    getWindowSettings :: a -> IO ContextSettings
    
    -- | Pop the event on top of events stack, if any, and return it.
    --
    -- This function is not blocking: if there's no pending event then
    -- it will return false and leave \a event unmodified.
    -- Note that more than one event may be present in the events stack,
    -- thus you should always call this function in a loop
    -- to make sure that you process every pending event.
    pollEvent :: a -> IO (Maybe SFEvent)
    
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
    waitEvent :: a -> IO (Maybe SFEvent)
    
    -- | Get the position of a window.
    getWindowPosition :: a -> IO Vec2i
    
    -- | Change the position of a window on screen.
    --
    -- This function only works for top-level windows
    -- (i.e. it will be ignored for windows created from
    -- the handle of a child window/control).
    setWindowPosition :: a -> Vec2i -> IO ()
    
    -- | Get the size of the rendering region of a window.
    --
    -- The size doesn't include the titlebar and borders
    -- of the window.
    getWindowSize :: a -> IO Vec2u
    
    -- | Change the size of the rendering region of a window.
    setWindowSize :: a -> Vec2u -> IO ()

    -- | Change the title of a window.
    setWindowTitle :: a -> String -> IO ()
    
    -- | Change a window's icon.
    --
    -- Pixels must be an array of width x height pixels
    -- in 32-bits RGBA format.
    setWindowIcon
        :: a
        -> Int   -- ^ Icon's width, in pixels
        -> Int   -- ^ Icon's height, in pixels
        -> Ptr b -- ^ Pixel data
        -> IO ()
    
    -- | Show or hide a window.
    setWindowVisible :: a -> Bool -> IO ()
    
    -- | Show or hide the mouse cursor.
    setMouseVisible :: a -> Bool -> IO ()
    
    -- | Enable or disable vertical synchronization.
    -- Activating vertical synchronization will limit the number
    -- of frames displayed to the refresh rate of the monitor.
    --
    -- This can avoid some visual artifacts, and limit the framerate
    -- to a good value (but not constant across different computers).
    setVSync :: a -> Bool -> IO ()
    
    -- | Enable or disable automatic key-repeat.
    --
    -- If key repeat is enabled, you will receive repeated
    -- KeyPress events while keeping a key pressed. If it is disabled,
    -- you will only get a single event when the key is pressed.
    --
    -- Key repeat is enabled by default.
    setKeyRepeat :: a -> Bool -> IO ()
    
    -- | Activate or deactivate a window as the current target for OpenGL rendering.
    --
    -- A window is active only on the current thread, if you want to
    -- make it active on another thread you have to deactivate it
    -- on the previous thread first if it was active.
    --
    -- Only one window can be active on a thread at a time, thus
    -- the window previously active (if any) automatically gets deactivated.
    setWindowActive :: a -> Bool -> IO ()
        
    -- | Limit the framerate to a maximum fixed frequency.
    --
    -- If a limit is set, the window will use a small delay after
    -- each call to 'display' to ensure that the current frame
    -- lasted long enough to match the framerate limit.
    setFramerateLimit :: a -> Int -> IO ()
    
    -- | Change the joystick threshold.
    --
    -- The joystick threshold is the value below which
    -- no JoyMoved event will be generated.
    setJoystickThreshold :: a -> Float -> IO ()
    
    -- | Get the OS-specific handle of the window.
    --
    -- The type of the returned handle is 'WindowHandle',
    -- which is a typedef to the handle type defined by the OS.
    --
    -- You shouldn't need to use this function, unless you have
    -- very specific stuff to implement that SFML doesn't support,
    -- or implement a temporary workaround until a bug is fixed.
    getSystemHandle :: a -> IO WindowHandle
    
    -- | Get the current position of the mouse
    --
    -- This function returns the current position of the mouse
    -- cursor relative to the given window, or desktop if 'Nothing' is passed.
    getMousePosition :: Maybe a -> IO Vec2i
    
    -- | Set the current position of the mouse
    --
    -- This function sets the current position of the mouse
    -- cursor relative to the given window, or desktop if 'Nothing' is passed.
    setMousePosition :: Vec2i -> Maybe a -> IO ()

