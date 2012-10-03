{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.System.Time
(
    Timeval
,   Time(..)
,   timeZero
,   asSeconds
,   asMilliseconds
,   asMicroseconds
,   seconds
,   milliseconds
,   microseconds
)
where


import Data.Int (Int64)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

#include <SFML/Config.h>

sizeInt64 = #{size sfInt64}

type Timeval = Int64

-- | Represents a time value
data Time = Time {-# UNPACK #-} !Timeval


instance Storable Time where
    sizeOf _ = sizeInt64
    alignment _ = alignment (undefined :: Int64)
    
    peek ptr = fmap Time $ peek (castPtr ptr)
    
    poke ptr (Time val) = poke (castPtr ptr) val


-- | Predefined "zero" time value.
timeZero :: Time
timeZero = unsafePerformIO $ alloca $ \ptr -> sfTime_Zero_helper ptr >> peek ptr

foreign import ccall "sfTime_Zero_helper"
    sfTime_Zero_helper :: Ptr Time -> IO ()

--CSFML_SYSTEM_API sfTime sfTime_Zero;


-- | Return a time value as a number of seconds.
asSeconds :: Time -> Float
asSeconds (Time t) = sfTime_asSeconds t

foreign import ccall "sfTime_asSeconds"
    sfTime_asSeconds :: Timeval -> Float

--CSFML_SYSTEM_API float sfTime_asSeconds(sfTime time);


-- | Return a time value as a number of milliseconds.
asMilliseconds :: Time -> Float
asMilliseconds (Time t) = sfTime_asMilliseconds t

foreign import ccall "sfTime_asMilliseconds"
    sfTime_asMilliseconds :: Timeval -> Float

--CSFML_SYSTEM_API sfInt32 sfTime_asMilliseconds(sfTime time);


-- | Return a time value as a number of microseconds.
asMicroseconds :: Time -> Float
asMicroseconds (Time t) = sfTime_asMicroseconds t

foreign import ccall "sfTime_asMicroseconds"
    sfTime_asMicroseconds :: Timeval -> Float

--CSFML_SYSTEM_API sfTimeval sfTime_asMicroseconds(sfTime time);


-- | Construct a time value from a number of seconds.
seconds
    :: Float -- ^ Number of seconds
    -> Time
seconds t = unsafePerformIO $ alloca $ \ptr -> sfSeconds_helper t ptr >> peek ptr

foreign import ccall "sfSeconds_helper"
    sfSeconds_helper :: Float -> Ptr Time -> IO ()

--CSFML_SYSTEM_API sfTime sfSeconds(float amount);


-- | Construct a time value from a number of milliseconds.
milliseconds
    :: Float -- ^ Number of milliseconds
    -> Time
milliseconds t = unsafePerformIO $ alloca $ \ptr -> sfMilliseconds_helper t ptr >> peek ptr

foreign import ccall "sfMilliseconds_helper"
    sfMilliseconds_helper :: Float -> Ptr Time -> IO ()

--CSFML_SYSTEM_API sfTime sfMilliseconds(sfInt32 amount);


-- | Construct a time value from a number of microseconds.
microseconds
    :: Float -- ^ Number of microseconds
    -> Time
microseconds t = unsafePerformIO $ alloca $ \ptr -> sfMicroseconds_helper t ptr >> peek ptr

foreign import ccall "sfMicroseconds_helper"
    sfMicroseconds_helper :: Float -> Ptr Time -> IO ()

--CSFML_SYSTEM_API sfTime sfMicroseconds(sfTimeval amount);

