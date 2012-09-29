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
        let ptr = castPtr ptr' :: Ptr CInt
        in do
            eventType <- peek ptr
            case eventType of
                0  -> return SFEvtClosed
                1  -> do
                    w <- peekByteOff ptr sizeInt
                    h <- peekByteOff ptr $ 2*sizeInt
                    return $ SFEvtResized w h
                2  -> return SFEvtLostFocus
                3  -> return SFEvtGainedFocus
                4  -> peekCAString (plusPtr ptr sizeInt) >>= return . SFEvtTextEntered
                5  -> do
                    keycode <- peekByteOff ptr sizeInt
                    let ptrChar = castPtr (advancePtr ptr 2) :: Ptr CChar
                    alt     <- peekByteOff ptrChar $ 0
                    ctrl    <- peekByteOff ptrChar $ sizeChar
                    shift   <- peekByteOff ptrChar $ 2*sizeChar
                    sys     <- peekByteOff ptrChar $ 3*sizeChar
                    return $ SFEvtKeyPressed keycode alt ctrl shift sys
                6  -> do
                    keycode <- peekByteOff ptr sizeInt
                    let ptrChar = castPtr (advancePtr ptr 2) :: Ptr CChar
                    alt     <- peekByteOff ptrChar $ 0
                    ctrl    <- peekByteOff ptrChar $ sizeChar
                    shift   <- peekByteOff ptrChar $ 2*sizeChar
                    sys     <- peekByteOff ptrChar $ 3*sizeChar
                    return $ SFEvtKeyReleased keycode alt ctrl shift sys
                7  -> do
                    delta <- peekByteOff ptr sizeInt
                    x     <- peekByteOff ptr $ 2*sizeInt
                    y     <- peekByteOff ptr $ 3*sizeInt
                    return $ SFEvtMouseWheelMoved delta x y
                8  -> do
                    button <- peekByteOff ptr sizeInt
                    x      <- peekByteOff ptr $ 2*sizeInt
                    y      <- peekByteOff ptr $ 3*sizeInt
                    return $ SFEvtMouseButtonPressed button x y
                9  -> do
                    button <- peekByteOff ptr sizeInt
                    x      <- peekByteOff ptr $ 2*sizeInt
                    y      <- peekByteOff ptr $ 3*sizeInt
                    return $ SFEvtMouseButtonReleased button x y
                10 -> do
                    x <- peekByteOff ptr sizeInt
                    y <- peekByteOff ptr $ 2*sizeInt
                    return $ SFEvtMouseMoved x y
                11 -> return SFEvtMouseEntered
                12 -> return SFEvtMouseLeft
                13 -> do
                    j  <- peekByteOff ptr sizeInt
                    bt <- peekByteOff ptr $ 2*sizeInt
                    return $ SFEvtJoystickButtonPressed j bt
                14 -> do
                    j  <- peekByteOff ptr sizeInt
                    bt <- peekByteOff ptr $ 2*sizeInt
                    return $ SFEvtJoystickButtonReleased j bt
                15 -> do
                    j    <- peekByteOff ptr sizeInt
                    axis <- fmap toEnum $ peekByteOff ptr $ 2*sizeInt
                    pos  <- peekByteOff ptr $ 3*sizeInt
                    return $ SFEvtJoystickMoved j axis pos
                16 -> peekByteOff ptr sizeInt >>= return . SFEvtJoystickConnected
                17 -> peekByteOff ptr sizeInt >>= return . SFEvtJoystickDisconnected
    
    poke ptr evt = return ()

