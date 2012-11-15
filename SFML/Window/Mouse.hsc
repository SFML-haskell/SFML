{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Mouse
(
    MouseButton(..)
,   isMouseButtonPressed
)
where


import SFML.Window.Types
import SFML.System.Vector2

import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable


data MouseButton
    = MouseLeft
    | MouseRight
    | MouseMiddle
    | MouseXButton1
    | MouseXButton2
    deriving (Eq, Enum, Bounded, Show)


sizeInt = #{size int}


instance Storable MouseButton where
    sizeOf _ = sizeInt
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = peek (castPtr ptr) >>= return . toEnum
    poke ptr bt = poke (castPtr ptr) (fromEnum bt)


-- | Check if a mouse button is pressed.
isMouseButtonPressed :: MouseButton -> IO Bool
isMouseButtonPressed bt = sfMouse_isButtonPressed (fromEnum bt) >>= return . (/=0)

foreign import ccall unsafe "sfMouse_isButtonPressed"
    sfMouse_isButtonPressed :: Int -> IO CChar

--CSFML_WINDOW_API sfBool sfMouse_isButtonPressed(sfMouseButton button);

