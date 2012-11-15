{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.System.Time
(
    Time
,   timeZero
,   asSeconds
,   asMilliseconds
,   asMicroseconds
,   seconds
,   milliseconds
,   microseconds
)
where


import Data.Int (Int64, Int32)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

#include <SFML/Config.h>

sizeInt64 = #{size sfInt64}

type Time = Int64


-- | Predefined "zero" time value.
timeZero :: Time
timeZero = 0

--CSFML_SYSTEM_API sfTime sfTime_Zero;


-- | Return a time value as a number of seconds.
asSeconds :: Time -> Float
asSeconds t = sfTime_asSeconds t

foreign import ccall unsafe "sfTime_asSeconds"
    sfTime_asSeconds :: Time -> Float

--CSFML_SYSTEM_API float sfTime_asSeconds(sfTime time);


-- | Return a time value as a number of milliseconds.
asMilliseconds :: Time -> Int
asMilliseconds t = fromIntegral $ sfTime_asMilliseconds t

foreign import ccall unsafe "sfTime_asMilliseconds"
    sfTime_asMilliseconds :: Time -> Int32

--CSFML_SYSTEM_API sfInt32 sfTime_asMilliseconds(sfTime time);


-- | Return a time value as a number of microseconds.
asMicroseconds :: Time -> Int64
asMicroseconds t = sfTime_asMicroseconds t

foreign import ccall unsafe "sfTime_asMicroseconds"
    sfTime_asMicroseconds :: Time -> Int64

--CSFML_SYSTEM_API sfInt64 sfTime_asMicroseconds(sfTime time);


-- | Construct a time value from a number of seconds.
seconds
    :: Float -- ^ Number of seconds
    -> Time
seconds t = unsafePerformIO $ alloca $ \ptr -> sfSeconds_helper t ptr >> peek ptr

foreign import ccall unsafe "sfSeconds_helper"
    sfSeconds_helper :: Float -> Ptr Time -> IO ()

--CSFML_SYSTEM_API sfTime sfSeconds(float amount);


-- | Construct a time value from a number of milliseconds.
milliseconds
    :: Int -- ^ Number of milliseconds
    -> Time
milliseconds t =
    unsafePerformIO $ alloca $ \ptr -> sfMilliseconds_helper (fromIntegral t) ptr >> peek ptr

foreign import ccall unsafe "sfMilliseconds_helper"
    sfMilliseconds_helper :: Int32 -> Ptr Time -> IO ()

--CSFML_SYSTEM_API sfTime sfMilliseconds(sfInt32 amount);


-- | Construct a time value from a number of microseconds.
microseconds
    :: Int64 -- ^ Number of microseconds
    -> Time
microseconds t = unsafePerformIO $ alloca $ \ptr -> sfMicroseconds_helper t ptr >> peek ptr

foreign import ccall unsafe "sfMicroseconds_helper"
    sfMicroseconds_helper :: Int64 -> Ptr Time -> IO ()

--CSFML_SYSTEM_API sfTime sfMicroseconds(sfInt64 amount);

