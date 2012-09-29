{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Mouse
(
    SFMouseButton(..)
,   isMouseButtonPressed
,   getMousePosition
,   setMousePosition
)
where


import SFML.Window.Types
import SFML.System.Vector2

import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable


data SFMouseButton
    = SFMouseLeft
    | SFMouseRight
    | SFMouseMiddle
    | SFMouseXButton1
    | SFMouseXButton2
    deriving (Eq, Enum, Bounded, Show)


sizeInt = #{size int}


instance Storable SFMouseButton where
    sizeOf _ = sizeInt
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = peek (castPtr ptr) >>= return . toEnum
    poke ptr bt = poke (castPtr ptr) (fromEnum bt)


-- | Check if a mouse button is pressed.
isMouseButtonPressed :: SFMouseButton -> IO Bool
isMouseButtonPressed bt = sfMouse_isButtonPressed (fromEnum bt) >>= return . (/=0)

foreign import ccall "sfMouse_isButtonPressed"
    sfMouse_isButtonPressed :: Int -> IO CChar

--CSFML_WINDOW_API sfBool sfMouse_isButtonPressed(sfMouseButton button);


-- | Get the current position of the mouse
--
-- This function returns the current position of the mouse
-- cursor relative to the given window, or desktop if NULL is passed.
getMousePosition :: SFWindow -> IO Vec2i
getMousePosition wnd = alloca $ \ptr -> sfMouse_getPosition_helper wnd ptr >> peek ptr

foreign import ccall "sfMouse_getPosition_helper"
    sfMouse_getPosition_helper :: SFWindow -> Ptr Vec2i -> IO ()

--CSFML_WINDOW_API sfVector2i sfMouse_getPosition(const sfWindow* relativeTo);


-- | Set the current position of the mouse
--
-- This function sets the current position of the mouse
-- cursor relative to the given window, or desktop if NULL is passed.
setMousePosition :: SFWindow -> IO ()
setMousePosition wnd = alloca $ \ptr -> sfMouse_setPosition_helper ptr wnd

foreign import ccall "sfMouse_setPosition_helper"
    sfMouse_setPosition_helper :: Ptr Vec2i -> SFWindow -> IO ()

--CSFML_WINDOW_API void sfMouse_setPosition(sfVector2i position, const sfWindow* relativeTo);

