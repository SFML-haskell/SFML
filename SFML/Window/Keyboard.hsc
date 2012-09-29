{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.Window.Keyboard
(
    SFKeyCode(..)
,   isKeyPressed
)
where


import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


-- | Key codes.
data SFKeyCode
    = SFKeyA            -- ^ The A key
    | SFKeyB            -- ^ The B key
    | SFKeyC            -- ^ The C key
    | SFKeyD            -- ^ The D key
    | SFKeyE            -- ^ The E key
    | SFKeyF            -- ^ The F key
    | SFKeyG            -- ^ The G key
    | SFKeyH            -- ^ The H key
    | SFKeyI            -- ^ The I key
    | SFKeyJ            -- ^ The J key
    | SFKeyK            -- ^ The K key
    | SFKeyL            -- ^ The L key
    | SFKeyM            -- ^ The M key
    | SFKeyN            -- ^ The N key
    | SFKeyO            -- ^ The O key
    | SFKeyP            -- ^ The P key
    | SFKeyQ            -- ^ The Q key
    | SFKeyR            -- ^ The R key
    | SFKeyS            -- ^ The S key
    | SFKeyT            -- ^ The T key
    | SFKeyU            -- ^ The U key
    | SFKeyV            -- ^ The V key
    | SFKeyW            -- ^ The W key
    | SFKeyX            -- ^ The X key
    | SFKeyY            -- ^ The Y key
    | SFKeyZ            -- ^ The Z key
    | SFKeyNum0         -- ^ The 0 key
    | SFKeyNum1         -- ^ The 1 key
    | SFKeyNum2         -- ^ The 2 key
    | SFKeyNum3         -- ^ The 3 key
    | SFKeyNum4         -- ^ The 4 key
    | SFKeyNum5         -- ^ The 5 key
    | SFKeyNum6         -- ^ The 6 key
    | SFKeyNum7         -- ^ The 7 key
    | SFKeyNum8         -- ^ The 8 key
    | SFKeyNum9         -- ^ The 9 key
    | SFKeyEscape       -- ^ The Escape key
    | SFKeyLControl     -- ^ The left Control key
    | SFKeyLShift       -- ^ The left Shift key
    | SFKeyLAlt         -- ^ The left Alt key
    | SFKeyLSystem      -- ^ The left OS specific key: window (Windows and Linux), apple (MacOS X), ...
    | SFKeyRControl     -- ^ The right Control key
    | SFKeyRShift       -- ^ The right Shift key
    | SFKeyRAlt         -- ^ The right Alt key
    | SFKeyRSystem      -- ^ The right OS specific key: window (Windows and Linux), apple (MacOS X), ...
    | SFKeyMenu         -- ^ The Menu key
    | SFKeyLBracket     -- ^ The [ key
    | SFKeyRBracket     -- ^ The ] key
    | SFKeySemiColon    -- ^ The ; key
    | SFKeyComma        -- ^ The , key
    | SFKeyPeriod       -- ^ The . key
    | SFKeyQuote        -- ^ The ' key
    | SFKeySlash        -- ^ The / key
    | SFKeyBackSlash    -- ^ The \ key
    | SFKeyTilde        -- ^ The ~ key
    | SFKeyEqual        -- ^ The = key
    | SFKeyDash         -- ^ The - key
    | SFKeySpace        -- ^ The Space key
    | SFKeyReturn       -- ^ The Return key
    | SFKeyBack         -- ^ The Backspace key
    | SFKeyTab          -- ^ The Tabulation key
    | SFKeyPageUp       -- ^ The Page up key
    | SFKeyPageDown     -- ^ The Page down key
    | SFKeyEnd          -- ^ The End key
    | SFKeyHome         -- ^ The Home key
    | SFKeyInsert       -- ^ The Insert key
    | SFKeyDelete       -- ^ The Delete key
    | SFKeyAdd          -- ^ +
    | SFKeySubtract     -- ^ -
    | SFKeyMultiply     -- ^ *
    | SFKeyDivide       -- ^ /
    | SFKeyLeft         -- ^ Left arrow
    | SFKeyRight        -- ^ Right arrow
    | SFKeyUp           -- ^ Up arrow
    | SFKeyDown         -- ^ Down arrow
    | SFKeyNumpad0      -- ^ The numpad 0 key
    | SFKeyNumpad1      -- ^ The numpad 1 key
    | SFKeyNumpad2      -- ^ The numpad 2 key
    | SFKeyNumpad3      -- ^ The numpad 3 key
    | SFKeyNumpad4      -- ^ The numpad 4 key
    | SFKeyNumpad5      -- ^ The numpad 5 key
    | SFKeyNumpad6      -- ^ The numpad 6 key
    | SFKeyNumpad7      -- ^ The numpad 7 key
    | SFKeyNumpad8      -- ^ The numpad 8 key
    | SFKeyNumpad9      -- ^ The numpad 9 key
    | SFKeyF1           -- ^ The F1 key
    | SFKeyF2           -- ^ The F2 key
    | SFKeyF3           -- ^ The F3 key
    | SFKeyF4           -- ^ The F4 key
    | SFKeyF5           -- ^ The F5 key
    | SFKeyF6           -- ^ The F6 key
    | SFKeyF7           -- ^ The F7 key
    | SFKeyF8           -- ^ The F8 key
    | SFKeyF9           -- ^ The F8 key
    | SFKeyF10          -- ^ The F10 key
    | SFKeyF11          -- ^ The F11 key
    | SFKeyF12          -- ^ The F12 key
    | SFKeyF13          -- ^ The F13 key
    | SFKeyF14          -- ^ The F14 key
    | SFKeyF15          -- ^ The F15 key
    | SFKeyPause        -- ^ The Pause key
    deriving (Eq, Enum, Bounded, Show)


sizeInt = #{size int}


instance Storable SFKeyCode where
    sizeOf _ = sizeInt
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = peek (castPtr ptr) >>= return . toEnum
    poke ptr bt = poke (castPtr ptr) (fromEnum bt)


-- | Check if a key is pressed
isKeyPressed :: SFKeyCode -> IO Bool
isKeyPressed k = sfKeyboard_isKeyPressed (fromEnum k) >>= return . (/=0)


foreign import ccall "sfKeyboard_isKeyPressed"
    sfKeyboard_isKeyPressed :: Int -> IO CChar

--CSFML_WINDOW_API sfBool sfKeyboard_isKeyPressed(sfKeyCode key);

