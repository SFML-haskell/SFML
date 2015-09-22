{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.JoystickIdentification
(
    JoystickIdentification(..)
)
where


import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr (Ptr, castPtr)

#include <SFML/Window/JoystickIdentification.h>


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t(y__); }, y__)


-- | Joystick's identification
data JoystickIdentification = JoystickIdentification
    { name      :: String
    , vendorId  :: Int
    , productId :: Int
    } deriving (Eq, Show)


instance Storable JoystickIdentification where
    sizeOf _ = #{size sfJoystickIdentification}
    alignment _ = #{alignment sfJoystickIdentification}

    peek ptr = JoystickIdentification
            <$> (#{peek sfJoystickIdentification, name} ptr >>= peekCString)
            <*> fmap fromIntegral (#{peek sfJoystickIdentification, vendorId} ptr :: IO CUInt)
            <*> fmap fromIntegral (#{peek sfJoystickIdentification, productId} ptr :: IO CUInt)

    poke ptr ji = do
        withCString (name ji) $ #{poke sfJoystickIdentification, name} ptr
        #{poke sfJoystickIdentification, vendorId}  ptr ((fromIntegral . vendorId)  ji :: CUInt)
        #{poke sfJoystickIdentification, productId} ptr ((fromIntegral . productId) ji :: CUInt)
