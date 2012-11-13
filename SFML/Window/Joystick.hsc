{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Joystick
(
    JoystickCap(..)
,   JoystickAxis(..)
,   isJoystickConnected
,   getButtonCount
,   hasAxis
,   isJoystickButtonPressed
,   getAxisPosition
,   updateJoystick
)
where


import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr (castPtr)


-- | Global joysticks capabilities
data JoystickCap
    = JoystickCount -- ^ Maximum number of supported joysticks
    | JoystickButtonCount -- ^ Maximum number of supported buttons
    | JoystickAxisCount -- ^ Maximum number of supported axes
    deriving (Eq, Bounded, Show)


instance Enum JoystickCap where
    
    fromEnum JoystickCount       = 8
    fromEnum JoystickButtonCount = 32
    fromEnum JoystickAxisCount   = 8
    
    toEnum 8  = JoystickCount
    toEnum 32 = JoystickButtonCount
    --toEnum 8  = JoystickAxisCount


-- | Axes supported by SFML joysticks
data JoystickAxis
    = JoystickX    -- ^ The X axis
    | JoystickY    -- ^ The Y axis
    | JoystickZ    -- ^ The Z axis
    | JoystickR    -- ^ The R axis
    | JoystickU    -- ^ The U axis
    | JoystickV    -- ^ The V axis
    | JoystickPovX -- ^ The X axis of the point-of-view hat
    | JoystickPovY -- ^ The Y axis of the point-of-view hat
    deriving (Eq, Enum, Bounded, Show)


sizeInt = #{size int}


instance Storable JoystickAxis where
    sizeOf _ = sizeInt
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = peek (castPtr ptr) >>= return . toEnum
    poke ptr bt = poke (castPtr ptr) (fromEnum bt)


-- | Check if a joystick is connected.
isJoystickConnected
    :: Int -- ^ Index of the joystick to check
    -> IO Bool

isJoystickConnected j = sfJoystick_isConnected (fromIntegral j) >>= return . (/=0)

foreign import ccall "sfJoystick_isConnected"
    sfJoystick_isConnected :: CUInt -> IO CChar

--CSFML_WINDOW_API sfBool sfJoystick_isConnected(unsigned int joystick);


-- | Return the number of buttons supported by a joystick.
--
-- If the joystick is not connected, this function returns 0.
getButtonCount
    :: Int -- ^ Index of the joystick
    -> IO Int

getButtonCount j = fmap fromIntegral $ sfJoystick_getButtonCount (fromIntegral j)

foreign import ccall "sfJoystick_getButtonCount"
    sfJoystick_getButtonCount :: CUInt -> IO CUInt

--CSFML_WINDOW_API unsigned int sfJoystick_getButtonCount(unsigned int joystick);


-- | Check if a joystick supports a given axis.
--
-- If the joystick is not connected, this function returns 'False'.
hasAxis
    :: Int -- ^ Index of the joystick
    -> Int -- ^ Axis to check
    -> IO Bool

hasAxis j a = sfJoystick_hasAxis (fromIntegral j) (fromIntegral a) >>= return . (/=0)

foreign import ccall "sfJoystick_hasAxis"
    sfJoystick_hasAxis :: CUInt -> CUInt -> IO CChar

--CSFML_WINDOW_API sfBool sfJoystick_hasAxis(unsigned int joystick, sfJoystickAxis axis);


-- | Check if a joystick button is pressed.
--
-- If the joystick is not connected, this function returns 'False'.
isJoystickButtonPressed
    :: Int -- ^ Index of the joystick
    -> Int -- ^ Button to check
    -> IO Bool

isJoystickButtonPressed j b = sfJoystick_isButtonPressed (fromIntegral j) (fromIntegral b) >>= return . (/=0)

foreign import ccall "sfJoystick_isButtonPressed"
    sfJoystick_isButtonPressed :: CUInt -> CUInt -> IO CChar

--CSFML_WINDOW_API sfBool sfJoystick_isButtonPressed(unsigned int joystick, unsigned int button);


-- | Get the current position of a joystick axis.
--
-- If the joystick is not connected, this function returns 0.
getAxisPosition
    :: Int -- ^ Index of the joystick
    -> Int -- ^ Axis to check
    -> IO Float

getAxisPosition j a = sfJoystick_getAxisPosition (fromIntegral j) (fromIntegral a)

foreign import ccall "sfJoystick_getAxisPosition"
    sfJoystick_getAxisPosition :: CUInt -> CUInt -> IO Float

--CSFML_WINDOW_API float sfJoystick_getAxisPosition(unsigned int joystick, sfJoystickAxis axis);


-- | Update the states of all joysticks.
--
-- This function is used internally by SFML, so you normally
-- don't have to call it explicitely. However, you may need to
-- call it if you have no window yet (or no window at all):
-- in this case the joysticks states are not updated automatically.
updateJoystick :: IO ()
updateJoystick = sfJoystick_update

foreign import ccall "sfJoystick_update"
    sfJoystick_update :: IO ()

--CSFML_WINDOW_API void sfJoystick_update(void);

