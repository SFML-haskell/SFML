{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module SFML.System.Clock
(
    SFClock(..)
,   createClock
,   copyClock
,   destroyClock
,   getElapsedTime
,   restartClock
)
where


import SFML.System.Time

import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable (peek)


newtype SFClock = SFClock (Ptr SFClock)


-- | Create a new clock and start it.
createClock :: IO SFClock
createClock = sfClock_create

foreign import ccall unsafe "sfClock_create"
    sfClock_create :: IO SFClock

--CSFML_SYSTEM_API sfClock* sfClock_create(void);


-- | Create a new clock by copying an existing one.
copyClock :: SFClock -> IO SFClock
copyClock = sfClock_copy

foreign import ccall unsafe "sfClock_copy"
    sfClock_copy :: SFClock -> IO SFClock

--CSFML_SYSTEM_API sfClock* sfClock_copy(sfClock* clock);


-- | Destroy a clock.
destroyClock :: SFClock -> IO ()
destroyClock = sfClock_destroy

foreign import ccall unsafe "sfClock_destroy"
    sfClock_destroy :: SFClock -> IO ()

--CSFML_SYSTEM_API void sfClock_destroy(sfClock* clock);


-- | Get the time elapsed in a clock.
--
-- This function returns the time elapsed since the last call
-- to sfClock_restart (or the construction of the object if
-- sfClock_restart has not been called).

getElapsedTime :: SFClock -> IO SFTime
getElapsedTime clock = alloca $ \ptr -> sfClock_getElapsedTime_helper clock ptr >> peek ptr

foreign import ccall unsafe "sfClock_getElapsedTime_helper"
    sfClock_getElapsedTime_helper :: SFClock -> Ptr SFTime -> IO Timeval

--CSFML_SYSTEM_API sfTime sfClock_getElapsedTime(const sfClock* clock);


-- | Restart a clock.
--
-- This function puts the time counter back to zero.
-- It also returns the time elapsed since the clock was started.

restartClock :: SFClock -> IO SFTime
restartClock clock = alloca $ \ptr -> sfClock_restart_helper clock ptr >> peek ptr

foreign import ccall unsafe "sfClock_restart_helper"
    sfClock_restart_helper :: SFClock -> Ptr SFTime -> IO ()

--CSFML_SYSTEM_API sfTime sfClock_restart(sfClock* clock);

