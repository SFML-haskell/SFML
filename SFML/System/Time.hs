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


type Timeval = Int64

-- | Represents a time value
data SFTime = SFTime {-# UNPACK #-} !Timeval


-- | Predefined "zero" time value.
timeZero :: SFTime
timeZero = SFTime sfTime_Zero

foreign import ccall "sfTime_Zero"
    sfTime_Zero :: Timeval

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
seconds = SFTime . sfSeconds

foreign import ccall "sfSeconds"
    sfSeconds :: Float -> Timeval

--CSFML_SYSTEM_API sfTime sfSeconds(float amount);


-- | Construct a time value from a number of milliseconds.
milliseconds
    :: Float -- ^ Number of milliseconds
    -> SFTime
milliseconds = SFTime . sfMilliseconds

foreign import ccall "sfMilliseconds"
    sfMilliseconds :: Float -> Timeval

--CSFML_SYSTEM_API sfTime sfMilliseconds(sfInt32 amount);


-- | Construct a time value from a number of microseconds.
microseconds
    :: Float -- ^ Number of microseconds
    -> SFTime
microseconds = SFTime . sfMicroseconds

foreign import ccall "sfMicroseconds"
    sfMicroseconds :: Float -> Timeval

--CSFML_SYSTEM_API sfTime sfMicroseconds(sfTimeval amount);

