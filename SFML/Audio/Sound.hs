module SFML.Audio.Sound
(
    createSound
,   copySound
,   destroy
,   play
,   pause
,   stop
,   setSoundBuffer
,   getSoundBuffer
,   setLoop
,   getLoop
,   getStatus
,   setPitch
,   setVolume
,   setPosition
,   setRelativeToListener
,   setMinDistance
,   setAttenuation
,   setPlayingOffset
,   getPitch
,   getVolume
,   getPosition
,   isRelativeToListener
,   getMinDistance
,   getAttenuation
,   getPlayingOffset
)
where


import SFML.Audio.SFSound
import SFML.Audio.SoundStatus
import SFML.Audio.Types
import SFML.SFResource
import SFML.System.Time
import SFML.System.Vector3

import Control.Monad ((>=>))
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)


-- | Create a new sound.
createSound :: IO Sound
createSound = sfSound_create

foreign import ccall unsafe "sfSound_create"
    sfSound_create :: IO Sound

-- CSFML_AUDIO_API sfSound* sfSound_create(void);


-- | Create a new sound by copying an existing one.
copySound :: Sound -> IO Sound
copySound = sfSound_copy

foreign import ccall unsafe "sfSound_copy"
    sfSound_copy :: Sound -> IO Sound

-- CSFML_AUDIO_API sfSound* sfSound_copy(sfSound* sound);


instance SFResource Sound where
    
    {-# INLINABLE destroy #-}
    destroy = sfSound_destroy

foreign import ccall unsafe "sfSound_destroy"
    sfSound_destroy :: Sound -> IO ()

-- CSFML_AUDIO_API void sfSound_destroy(sfSound* sound);


-- | Set the source buffer containing the audio data to play.
-- 
-- It is important to note that the sound buffer is not copied,
-- thus the sfSoundBuffer object must remain alive as long
-- as it is attached to the sound.
setSoundBuffer :: Sound -> SoundBuffer -> IO ()
setSoundBuffer = sfSound_setBuffer

foreign import ccall unsafe "sfSound_setBuffer"
    sfSound_setBuffer :: Sound -> SoundBuffer -> IO ()

-- CSFML_AUDIO_API void sfSound_setBuffer(sfSound* sound, const sfSoundBuffer* buffer);


-- | Get the audio buffer attached to a sound.
getSoundBuffer :: Sound -> IO SoundBuffer
getSoundBuffer = sfSound_getBuffer

foreign import ccall unsafe "sfSound_getBuffer"
    sfSound_getBuffer :: Sound -> IO SoundBuffer

-- CSFML_AUDIO_API const sfSoundBuffer* sfSound_getBuffer(const sfSound* sound);


instance SFSound Sound where
    
    {-# INLINABLE play #-}
    play = sfSound_play
    
    {-# INLINABLE pause #-}
    pause = sfSound_pause
    
    {-# INLINABLE stop #-}
    stop = sfSound_stop
    
    {-# INLINABLE getAttenuation #-}
    getAttenuation = sfSound_getAttenuation
    
    {-# INLINABLE getLoop #-}
    getLoop = sfSound_getLoop >=> return . toEnum . fromIntegral
    
    {-# INLINABLE getMinDistance #-}
    getMinDistance = sfSound_getMinDistance
    
    {-# INLINABLE getPitch #-}
    getPitch = sfSound_getPitch
    
    {-# INLINABLE getPlayingOffset #-}
    getPlayingOffset sound = alloca $ \ptr -> sfSound_getPlayingOffset_helper sound ptr >> peek ptr
    
    {-# INLINABLE getPosition #-}
    getPosition sound = alloca $ \ptr -> sfSound_getPosition_helper sound ptr >> peek ptr
    
    {-# INLINABLE getStatus #-}
    getStatus = sfSound_getStatus >=> return . toEnum . fromIntegral
    
    {-# INLINABLE getVolume #-}
    getVolume = sfSound_getVolume
    
    {-# INLINABLE isRelativeToListener #-}
    isRelativeToListener = sfSound_isRelativeToListener >=> return . toEnum . fromIntegral
    
    {-# INLINABLE setAttenuation #-}
    setAttenuation = sfSound_setAttenuation
    
    {-# INLINABLE setLoop #-}
    setLoop sound val = sfSound_setLoop sound (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setMinDistance #-}
    setMinDistance = sfSound_setMinDistance
    
    {-# INLINABLE setPitch #-}
    setPitch = sfSound_setPitch
    
    {-# INLINABLE setPlayingOffset #-}
    setPlayingOffset = sfSound_setPlayingOffset
    
    {-# INLINABLE setPosition #-}
    setPosition sound pos = with pos $ sfSound_setPosition_helper sound
    
    {-# INLINABLE setRelativeToListener #-}
    setRelativeToListener sound val = sfSound_setRelativeToListener sound (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setVolume #-}
    setVolume = sfSound_setVolume


foreign import ccall unsafe "sfSound_play"
    sfSound_play :: Sound -> IO ()

-- CSFML_AUDIO_API void sfSound_play(sfSound* sound);

foreign import ccall unsafe "sfSound_pause"
    sfSound_pause :: Sound -> IO ()

-- CSFML_AUDIO_API void sfSound_pause(sfSound* sound);

foreign import ccall unsafe "sfSound_stop"
    sfSound_stop :: Sound -> IO ()

-- CSFML_AUDIO_API void sfSound_stop(sfSound* sound);

foreign import ccall unsafe "sfSound_getAttenuation"
    sfSound_getAttenuation :: Sound -> IO Float

-- CSFML_AUDIO_API float sfSound_getAttenuation(const sfSound* sound);

foreign import ccall unsafe "sfSound_getLoop"
    sfSound_getLoop :: Sound -> IO CInt

-- CSFML_AUDIO_API sfBool sfSound_getLoop(const sfSound* sound);

foreign import ccall unsafe "sfSound_getMinDistance"
    sfSound_getMinDistance :: Sound -> IO Float

-- CSFML_AUDIO_API float sfSound_getMinDistance(const sfSound* sound);

foreign import ccall unsafe "sfSound_getPitch"
    sfSound_getPitch :: Sound -> IO Float

-- CSFML_AUDIO_API float sfSound_getPitch(const sfSound* sound);

foreign import ccall unsafe "sfSound_getPlayingOffset_helper"
    sfSound_getPlayingOffset_helper :: Sound -> Ptr Time -> IO ()

-- CSFML_AUDIO_API sfTime sfSound_getPlayingOffset(const sfSound* sound);

foreign import ccall unsafe "sfSound_getPosition_helper"
    sfSound_getPosition_helper :: Sound -> Ptr Vec3f -> IO ()

-- CSFML_AUDIO_API sfVector3f sfSound_getPosition(const sfSound* sound);

foreign import ccall unsafe "sfSound_getStatus"
    sfSound_getStatus :: Sound -> IO CInt

-- CSFML_AUDIO_API sfSoundStatus sfSound_getStatus(const sfSound* sound);

foreign import ccall unsafe "sfSound_getVolume"
    sfSound_getVolume :: Sound -> IO Float

-- CSFML_AUDIO_API float sfSound_getVolume(const sfSound* sound);

foreign import ccall unsafe "sfSound_isRelativeToListener"
    sfSound_isRelativeToListener :: Sound -> IO CInt

-- CSFML_AUDIO_API sfBool sfSound_isRelativeToListener(const sfSound* sound);

foreign import ccall unsafe "sfSound_setAttenuation"
    sfSound_setAttenuation :: Sound -> Float -> IO ()

-- CSFML_AUDIO_API void sfSound_setAttenuation(sfSound* sound, float attenuation);

foreign import ccall unsafe "sfSound_setLoop"
    sfSound_setLoop :: Sound -> CInt -> IO ()

-- CSFML_AUDIO_API void sfSound_setLoop(sfSound* sound, sfBool loop);

foreign import ccall unsafe "sfSound_setMinDistance"
    sfSound_setMinDistance :: Sound -> Float -> IO ()

-- CSFML_AUDIO_API void sfSound_setMinDistance(sfSound* sound, float distance);

foreign import ccall unsafe "sfSound_setPitch"
    sfSound_setPitch :: Sound -> Float -> IO ()

-- CSFML_AUDIO_API void sfSound_setPitch(sfSound* sound, float pitch);

foreign import ccall unsafe "sfSound_setPlayingOffset"
    sfSound_setPlayingOffset :: Sound -> Time -> IO ()

-- CSFML_AUDIO_API void sfSound_setPlayingOffset(sfSound* sound, sfTime timeOffset);

foreign import ccall unsafe "sfSound_setPosition_helper"
    sfSound_setPosition_helper :: Sound -> Ptr Vec3f -> IO ()

-- CSFML_AUDIO_API void sfSound_setPosition(sfSound* sound, sfVector3f position);

foreign import ccall unsafe "sfSound_setRelativeToListener"
    sfSound_setRelativeToListener :: Sound -> CInt -> IO ()

-- CSFML_AUDIO_API void sfSound_setRelativeToListener(sfSound* sound, sfBool relative);

foreign import ccall unsafe "sfSound_setVolume"
    sfSound_setVolume :: Sound -> Float -> IO ()

-- CSFML_AUDIO_API void sfSound_setVolume(sfSound* sound, float volume);

