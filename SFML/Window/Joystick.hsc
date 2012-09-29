{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Joystick
(
    SFJoystickCap(..)
,   SFJoystickAxis(..)
,   isJoystickConnected
,   getButtonCount
,   hasAxis
,   isJoystickButtonPressed
,   getAxisPosition
,   updateJoystick
)
where


import Foreign.C.Types


-- | Global joysticks capabilities
data SFJoystickCap
    = SFJoystickCount
    | SFJoystickButtonCount
    | SFJoystickAxisCount
    deriving (Eq, Bounded, Show)


instance Enum SFJoystickCap where
    
    fromEnum SFJoystickCount       = 8
    fromEnum SFJoystickButtonCount = 32
    fromEnum SFJoystickAxisCount   = 8
    
    toEnum 8  = SFJoystickCount
    toEnum 32 = SFJoystickButtonCount
    --toEnum 8  = SFJoystickAxisCount


-- | Axes supported by SFML joysticks
data SFJoystickAxis
    = SFJoystickX    -- ^ The X axis
    | SFJoystickY    -- ^ The Y axis
    | SFJoystickZ    -- ^ The Z axis
    | SFJoystickR    -- ^ The R axis
    | SFJoystickU    -- ^ The U axis
    | SFJoystickV    -- ^ The V axis
    | SFJoystickPovX -- ^ The X axis of the point-of-view hat
    | SFJoystickPovY -- ^ The Y axis of the point-of-view hat
    deriving (Eq, Enum, Bounded, Show)


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
-- If the joystick is not connected, this function returns false.
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
-- If the joystick is not connected, this function returns false.
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

