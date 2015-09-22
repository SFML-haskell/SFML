{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Mouse
(
    MouseButton(..)
,   MouseWheel(..)
,   isMouseButtonPressed
)
where


import SFML.Window.Types
import SFML.System.Vector2

import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable

#include <SFML/Window/Mouse.h>


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t(y__); }, y__)


data MouseButton
    = MouseLeft
    | MouseRight
    | MouseMiddle
    | MouseXButton1
    | MouseXButton2
    deriving (Eq, Enum, Bounded, Show)


data MouseWheel
    = MouseVerticalWheel   -- ^ The vertical mouse wheel
    | MouseHorizontalWheel -- ^ The horizontal mouse wheel
    deriving (Eq, Enum, Bounded, Show)


instance Storable MouseWheel where
    sizeOf _ = #{size sfMouseWheel}
    alignment _ = #{alignment sfMouseWheel}

    peek ptr = peek (castPtr ptr :: Ptr CInt) >>= return . toEnum . fromIntegral
    poke ptr mw = poke (castPtr ptr :: Ptr CInt) (fromIntegral . fromEnum $ mw)


instance Storable MouseButton where
    sizeOf _ = #{size sfMouseButton}
    alignment _ = #{alignment sfMouseButton}

    peek ptr = peek (castPtr ptr :: Ptr CInt) >>= return . toEnum . fromIntegral
    poke ptr bt = poke (castPtr ptr :: Ptr CInt) (fromIntegral . fromEnum $ bt)


-- | Check if a mouse button is pressed.
isMouseButtonPressed :: MouseButton -> IO Bool
isMouseButtonPressed bt = sfMouse_isButtonPressed (fromEnum bt) >>= return . (/=0)

foreign import ccall unsafe "sfMouse_isButtonPressed"
    sfMouse_isButtonPressed :: Int -> IO CChar

--CSFML_WINDOW_API sfBool sfMouse_isButtonPressed(sfMouseButton button);

