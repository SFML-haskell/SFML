{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.System.Time
(
    Timeval
,   SFTime(..)
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
data SFTime = SFTime {-# UNPACK #-} !Timeval


instance Storable SFTime where
    sizeOf _ = sizeInt64
    alignment _ = alignment (undefined :: Int64)
    
    peek ptr = fmap SFTime $ peek (castPtr ptr)
    
    poke ptr (SFTime val) = poke (castPtr ptr) val


-- | Predefined "zero" time value.
timeZero :: SFTime
timeZero = unsafePerformIO $ alloca $ \ptr -> sfTime_Zero_helper ptr >> peek ptr

foreign import ccall "sfTime_Zero_helper"
    sfTime_Zero_helper :: Ptr SFTime -> IO ()

--CSFML_SYSTEM_API sfTime sfTime_Zero;


-- | Return a time value as a number of seconds.
asSeconds :: SFTime -> Float
asSeconds (SFTime t) = sfTime_asSeconds t

foreign import ccall "sfTime_asSeconds"
    sfTime_asSeconds :: Timeval -> Float

--CSFML_SYSTEM_API float sfTime_asSeconds(sfTime time);


-- | Return a time value as a number of milliseconds.
asMilliseconds :: SFTime -> Float
asMilliseconds (SFTime t) = sfTime_asMilliseconds t

foreign import ccall "sfTime_asMilliseconds"
    sfTime_asMilliseconds :: Timeval -> Float

--CSFML_SYSTEM_API sfInt32 sfTime_asMilliseconds(sfTime time);


-- | Return a time value as a number of microseconds.
asMicroseconds :: SFTime -> Float
asMicroseconds (SFTime t) = sfTime_asMicroseconds t

foreign import ccall "sfTime_asMicroseconds"
    sfTime_asMicroseconds :: Timeval -> Float

--CSFML_SYSTEM_API sfTimeval sfTime_asMicroseconds(sfTime time);


-- | Construct a time value from a number of seconds.
seconds
    :: Float -- ^ Number of seconds
    -> SFTime
seconds t = unsafePerformIO $ alloca $ \ptr -> sfSeconds_helper t ptr >> peek ptr

foreign import ccall "sfSeconds_helper"
    sfSeconds_helper :: Float -> Ptr SFTime -> IO ()

--CSFML_SYSTEM_API sfTime sfSeconds(float amount);


-- | Construct a time value from a number of milliseconds.
milliseconds
    :: Float -- ^ Number of milliseconds
    -> SFTime
milliseconds t = unsafePerformIO $ alloca $ \ptr -> sfMilliseconds_helper t ptr >> peek ptr

foreign import ccall "sfMilliseconds_helper"
    sfMilliseconds_helper :: Float -> Ptr SFTime -> IO ()

--CSFML_SYSTEM_API sfTime sfMilliseconds(sfInt32 amount);


-- | Construct a time value from a number of microseconds.
microseconds
    :: Float -- ^ Number of microseconds
    -> SFTime
microseconds t = unsafePerformIO $ alloca $ \ptr -> sfMicroseconds_helper t ptr >> peek ptr

foreign import ccall "sfMicroseconds_helper"
    sfMicroseconds_helper :: Float -> Ptr SFTime -> IO ()

--CSFML_SYSTEM_API sfTime sfMicroseconds(sfTimeval amount);

