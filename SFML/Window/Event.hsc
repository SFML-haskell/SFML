{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Event
(
    SFEvent(..)
)
where


import SFML.Window.Joystick
import SFML.Window.Keyboard
import SFML.Window.Mouse

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

#include <SFML/Window/Event.h>


data SFEvent
    = SFEvtClosed
    | SFEvtResized
    { width  :: Int
    , height :: Int
    }
    | SFEvtLostFocus
    | SFEvtGainedFocus
    | SFEvtTextEntered
    { text :: String
    }
    | SFEvtKeyPressed
    { code  :: SFKeyCode
    , alt   :: Bool
    , ctrl  :: Bool
    , shift :: Bool
    , sys   :: Bool
    }
    | SFEvtKeyReleased
    { code  :: SFKeyCode
    , alt   :: Bool
    , ctrl  :: Bool
    , shift :: Bool
    , sys   :: Bool
    }
    | SFEvtMouseWheelMoved
    { delta :: Int
    , x     :: Int
    , y     :: Int
    }
    | SFEvtMouseButtonPressed
    { button :: SFMouseButton
    , x      :: Int
    , y      :: Int
    }
    | SFEvtMouseButtonReleased
    { button :: SFMouseButton
    , x      :: Int
    , y      :: Int
    }
    | SFEvtMouseMoved
    { x :: Int
    , y :: Int
    }
    | SFEvtMouseEntered
    | SFEvtMouseLeft
    | SFEvtJoystickButtonPressed
    { joystickId :: Int
    , joystickBt :: Int
    }
    | SFEvtJoystickButtonReleased
    { joystickId :: Int
    , joystickBt :: Int
    }
    | SFEvtJoystickMoved
    { joystickId   :: Int
    , joystickAxis :: SFJoystickAxis
    , position     :: Float
    }
    | SFEvtJoystickConnected
    { joystickId :: Int
    }
    | SFEvtJoystickDisconnected
    { joystickId :: Int
    }


sizeInt = #{size int}
sizeChar = #{size char}


instance Storable SFEvent where
    sizeOf _ = #{size sfEvent}
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr' =
        let ptr'' = castPtr ptr' :: Ptr CInt
        in do
            let ptr = castPtr ptr''
            eventType <- peek ptr''
            case eventType of
                0  -> return SFEvtClosed
                1  -> do
                    w <- #{peek sfSizeEvent, width} ptr
                    h <- #{peek sfSizeEvent, height} ptr
                    return $ SFEvtResized w h
                2  -> return SFEvtLostFocus
                3  -> return SFEvtGainedFocus
                4  -> peekCAString (plusPtr ptr sizeInt) >>= return . SFEvtTextEntered
                5  -> do
                    code  <- #{peek sfKeyEvent, code} ptr
                    alt   <- #{peek sfKeyEvent, alt} ptr
                    ctrl  <- #{peek sfKeyEvent, control} ptr
                    shift <- #{peek sfKeyEvent, shift} ptr
                    sys   <- #{peek sfKeyEvent, system} ptr
                    return $ SFEvtKeyPressed code alt ctrl shift sys
                6  -> do
                    code  <- #{peek sfKeyEvent, code} ptr
                    alt   <- #{peek sfKeyEvent, alt} ptr
                    ctrl  <- #{peek sfKeyEvent, control} ptr
                    shift <- #{peek sfKeyEvent, shift} ptr
                    sys   <- #{peek sfKeyEvent, system} ptr
                    return $ SFEvtKeyReleased code alt ctrl shift sys
                7  -> do
                    delta <- #{peek sfMouseWheelEvent, delta} ptr
                    x     <- #{peek sfMouseWheelEvent, x} ptr
                    y     <- #{peek sfMouseWheelEvent, y} ptr
                    return $ SFEvtMouseWheelMoved delta x y
                8  -> do
                    button <- #{peek sfMouseButtonEvent, button} ptr
                    x      <- #{peek sfMouseButtonEvent, x} ptr
                    y      <- #{peek sfMouseButtonEvent, y} ptr
                    return $ SFEvtMouseButtonPressed button x y
                9  -> do
                    button <- #{peek sfMouseButtonEvent, button} ptr
                    x      <- #{peek sfMouseButtonEvent, x} ptr
                    y      <- #{peek sfMouseButtonEvent, y} ptr
                    return $ SFEvtMouseButtonReleased button x y
                10 -> do
                    x <- #{peek sfMouseMoveEvent, x} ptr
                    y <- #{peek sfMouseMoveEvent, y} ptr
                    return $ SFEvtMouseMoved x y
                11 -> return SFEvtMouseEntered
                12 -> return SFEvtMouseLeft
                13 -> do
                    j  <- #{peek sfJoystickButtonEvent, joystickId} ptr
                    bt <- #{peek sfJoystickButtonEvent, button} ptr
                    return $ SFEvtJoystickButtonPressed j bt
                14 -> do
                    j  <- #{peek sfJoystickButtonEvent, joystickId} ptr
                    bt <- #{peek sfJoystickButtonEvent, button} ptr
                    return $ SFEvtJoystickButtonReleased j bt
                15 -> do
                    j    <- #{peek sfJoystickMoveEvent, joystickId} ptr
                    axis <- #{peek sfJoystickMoveEvent, axis} ptr
                    pos  <- #{peek sfJoystickMoveEvent, position} ptr
                    return $ SFEvtJoystickMoved j axis pos
                16 -> peekByteOff ptr sizeInt >>= return . SFEvtJoystickConnected
                17 -> peekByteOff ptr sizeInt >>= return . SFEvtJoystickDisconnected
    
    poke ptr evt = return ()

