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
    { code  :: KeyCode
    , alt   :: Bool
    , ctrl  :: Bool
    , shift :: Bool
    , sys   :: Bool
    }
    | SFEvtKeyReleased
    { code  :: KeyCode
    , alt   :: Bool
    , ctrl  :: Bool
    , shift :: Bool
    , sys   :: Bool
    }
    | SFEvtMouseWheelMoved
    { moveDelta :: Int
    , x         :: Int
    , y         :: Int
    }
    | SFEvtMouseWheelScrolled
    { wheel       :: MouseWheel
    , scrollDelta :: Float
    , x           :: Int
    , y           :: Int
    }
    | SFEvtMouseButtonPressed
    { button :: MouseButton
    , x      :: Int
    , y      :: Int
    }
    | SFEvtMouseButtonReleased
    { button :: MouseButton
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
    , joystickAxis :: JoystickAxis
    , position     :: Float
    }
    | SFEvtJoystickConnected
    { joystickId :: Int
    }
    | SFEvtJoystickDisconnected
    { joystickId :: Int
    }
    deriving (Eq, Show)


sizeInt = #{size int}
sizeChar = #{size char}


instance Storable SFEvent where
    sizeOf _ = #{size sfEvent}
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr' =
        let ptr'' = castPtr ptr' :: Ptr CInt
        in do
            let ptr = castPtr ptr''
            eventType <- peek ptr'' :: IO CInt
            case eventType of
                0  -> return SFEvtClosed
                1  -> do
                    w <- #{peek sfSizeEvent, width} ptr  :: IO CUInt
                    h <- #{peek sfSizeEvent, height} ptr :: IO CUInt
                    return $ SFEvtResized (fromIntegral w) (fromIntegral h)
                2  -> return SFEvtLostFocus
                3  -> return SFEvtGainedFocus
                4  -> peekCAString (plusPtr ptr sizeInt) >>= return . SFEvtTextEntered
                5  -> do
                    code  <- #{peek sfKeyEvent, code} ptr
                    alt   <- #{peek sfKeyEvent, alt} ptr :: IO CInt
                    ctrl  <- #{peek sfKeyEvent, control} ptr :: IO CInt
                    shift <- #{peek sfKeyEvent, shift} ptr :: IO CInt
                    sys   <- #{peek sfKeyEvent, system} ptr :: IO CInt
                    return $ SFEvtKeyPressed code (toEnum . fromIntegral $ alt) (toEnum . fromIntegral $ ctrl)
                               (toEnum . fromIntegral $ shift) (toEnum . fromIntegral $ sys)
                6  -> do
                    code  <- #{peek sfKeyEvent, code} ptr
                    alt   <- #{peek sfKeyEvent, alt} ptr :: IO CInt
                    ctrl  <- #{peek sfKeyEvent, control} ptr :: IO CInt
                    shift <- #{peek sfKeyEvent, shift} ptr :: IO CInt
                    sys   <- #{peek sfKeyEvent, system} ptr :: IO CInt
                    return $ SFEvtKeyReleased code (toEnum . fromIntegral $ alt) (toEnum . fromIntegral $ ctrl)
                               (toEnum . fromIntegral $ shift) (toEnum . fromIntegral $ sys)
                7  -> do
                    delta <- #{peek sfMouseWheelEvent, delta} ptr :: IO CInt
                    x     <- #{peek sfMouseWheelEvent, x} ptr :: IO CInt
                    y     <- #{peek sfMouseWheelEvent, y} ptr :: IO CInt
                    return $ SFEvtMouseWheelMoved (fromIntegral delta) (fromIntegral x) (fromIntegral y)
                8 -> do
                    wheel <- #{peek sfMouseWheelScrollEvent, wheel} ptr :: IO MouseWheel
                    delta <- #{peek sfMouseWheelScrollEvent, delta} ptr :: IO CFloat
                    x     <- #{peek sfMouseWheelScrollEvent, x} ptr :: IO CInt
                    y     <- #{peek sfMouseWheelScrollEvent, y} ptr :: IO CInt
                    return $ SFEvtMouseWheelScrolled wheel (realToFrac delta) (fromIntegral x) (fromIntegral y)
                9  -> do
                    button <- #{peek sfMouseButtonEvent, button} ptr
                    x      <- #{peek sfMouseButtonEvent, x} ptr :: IO CInt
                    y      <- #{peek sfMouseButtonEvent, y} ptr :: IO CInt
                    return $ SFEvtMouseButtonPressed button (fromIntegral x) (fromIntegral y)
                10 -> do
                    button <- #{peek sfMouseButtonEvent, button} ptr
                    x      <- #{peek sfMouseButtonEvent, x} ptr :: IO CInt
                    y      <- #{peek sfMouseButtonEvent, y} ptr :: IO CInt
                    return $ SFEvtMouseButtonReleased button (fromIntegral x) (fromIntegral y)
                11 -> do
                    x <- #{peek sfMouseMoveEvent, x} ptr :: IO CInt
                    y <- #{peek sfMouseMoveEvent, y} ptr :: IO CInt
                    return $ SFEvtMouseMoved (fromIntegral x) (fromIntegral y)
                12 -> return SFEvtMouseEntered
                13 -> return SFEvtMouseLeft
                14 -> do
                    j  <- #{peek sfJoystickButtonEvent, joystickId} ptr :: IO CUInt
                    bt <- #{peek sfJoystickButtonEvent, button} ptr :: IO CUInt
                    return $ SFEvtJoystickButtonPressed (fromIntegral j) (fromIntegral bt)
                15 -> do
                    j  <- #{peek sfJoystickButtonEvent, joystickId} ptr :: IO CUInt
                    bt <- #{peek sfJoystickButtonEvent, button} ptr :: IO CUInt
                    return $ SFEvtJoystickButtonReleased (fromIntegral j) (fromIntegral bt)
                16 -> do
                    j    <- #{peek sfJoystickMoveEvent, joystickId} ptr :: IO CUInt
                    axis <- #{peek sfJoystickMoveEvent, axis} ptr
                    pos  <- fmap realToFrac (#{peek sfJoystickMoveEvent, position} ptr :: IO CFloat)
                    return $ SFEvtJoystickMoved (fromIntegral j) axis pos
                17 -> peekByteOff ptr sizeInt >>= return . SFEvtJoystickConnected
                18 -> peekByteOff ptr sizeInt >>= return . SFEvtJoystickDisconnected
    
    poke ptr evt = return ()

