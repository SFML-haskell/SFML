{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Event
(
    SFEvent(..)
)
where


import SFML.Window.Joystick
import SFML.Window.Keyboard
import SFML.Window.Mouse

import Control.Applicative ((<$>), (<*>))
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
                1  -> SFEvtResized
                   <$> fmap fromIntegral (#{peek sfSizeEvent, width}  ptr :: IO CUInt)
                   <*> fmap fromIntegral (#{peek sfSizeEvent, height} ptr :: IO CUInt)
                2  -> return SFEvtLostFocus
                3  -> return SFEvtGainedFocus
                4  -> peekCAString (plusPtr ptr sizeInt) >>= return . SFEvtTextEntered
                5  -> SFEvtKeyPressed
                   <$> #{peek sfKeyEvent, code} ptr
                   <*> fmap (toEnum . fromIntegral) (#{peek sfKeyEvent, alt} ptr :: IO CInt)
                   <*> fmap (toEnum . fromIntegral) (#{peek sfKeyEvent, control} ptr :: IO CInt)
                   <*> fmap (toEnum . fromIntegral) (#{peek sfKeyEvent, shift} ptr :: IO CInt)
                   <*> fmap (toEnum . fromIntegral) (#{peek sfKeyEvent, system} ptr :: IO CInt)
                6  -> SFEvtKeyReleased
                   <$> #{peek sfKeyEvent, code} ptr
                   <*> fmap (toEnum . fromIntegral) (#{peek sfKeyEvent, alt} ptr :: IO CInt)
                   <*> fmap (toEnum . fromIntegral) (#{peek sfKeyEvent, control} ptr :: IO CInt)
                   <*> fmap (toEnum . fromIntegral) (#{peek sfKeyEvent, shift} ptr :: IO CInt)
                   <*> fmap (toEnum . fromIntegral) (#{peek sfKeyEvent, system} ptr :: IO CInt)
                7  -> SFEvtMouseWheelMoved
                   <$> fmap fromIntegral (#{peek sfMouseWheelEvent, delta} ptr :: IO CInt)
                   <*> fmap fromIntegral (#{peek sfMouseWheelEvent, x} ptr :: IO CInt)
                   <*> fmap fromIntegral (#{peek sfMouseWheelEvent, y} ptr :: IO CInt)
                8 -> SFEvtMouseWheelScrolled
                  <$> (#{peek sfMouseWheelScrollEvent, wheel} ptr :: IO MouseWheel)
                  <*> fmap realToFrac   (#{peek sfMouseWheelScrollEvent, delta} ptr :: IO CFloat)
                  <*> fmap fromIntegral (#{peek sfMouseWheelScrollEvent, x} ptr :: IO CInt)
                  <*> fmap fromIntegral (#{peek sfMouseWheelScrollEvent, y} ptr :: IO CInt)
                9  -> SFEvtMouseButtonPressed
                   <$> #{peek sfMouseButtonEvent, button} ptr
                   <*> fmap fromIntegral (#{peek sfMouseButtonEvent, x} ptr :: IO CInt)
                   <*> fmap fromIntegral (#{peek sfMouseButtonEvent, y} ptr :: IO CInt)
                10 -> SFEvtMouseButtonReleased
                   <$> #{peek sfMouseButtonEvent, button} ptr
                   <*> fmap fromIntegral (#{peek sfMouseButtonEvent, x} ptr :: IO CInt)
                   <*> fmap fromIntegral (#{peek sfMouseButtonEvent, y} ptr :: IO CInt)
                11 -> SFEvtMouseMoved
                   <$> fmap fromIntegral (#{peek sfMouseMoveEvent, x} ptr :: IO CInt)
                   <*> fmap fromIntegral (#{peek sfMouseMoveEvent, y} ptr :: IO CInt)
                12 -> return SFEvtMouseEntered
                13 -> return SFEvtMouseLeft
                14 -> SFEvtJoystickButtonPressed
                   <$> fmap fromIntegral (#{peek sfJoystickButtonEvent, joystickId} ptr :: IO CUInt)
                   <*> fmap fromIntegral (#{peek sfJoystickButtonEvent, button} ptr :: IO CUInt)
                15 -> SFEvtJoystickButtonReleased
                   <$> fmap fromIntegral (#{peek sfJoystickButtonEvent, joystickId} ptr :: IO CUInt)
                   <*> fmap fromIntegral (#{peek sfJoystickButtonEvent, button} ptr :: IO CUInt)
                16 -> SFEvtJoystickMoved
                   <$> fmap fromIntegral (#{peek sfJoystickMoveEvent, joystickId} ptr :: IO CUInt)
                   <*> #{peek sfJoystickMoveEvent, axis} ptr
                   <*> fmap realToFrac (#{peek sfJoystickMoveEvent, position} ptr :: IO CFloat)
                17 -> peekByteOff ptr sizeInt >>= return . SFEvtJoystickConnected
                18 -> peekByteOff ptr sizeInt >>= return . SFEvtJoystickDisconnected

    poke ptr evt = return ()

