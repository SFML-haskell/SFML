module SFML.Audio.Music
(
    musicFromFile
,   musicFromMemory
,   musicFromStream
,   destroy
,   setLoop
,   getLoop
,   getDuration
,   play
,   pause
,   stop
,   getChannelCount
,   getSampleRate
,   getStatus
,   getPlayingOffset
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
)
where


import SFML.Audio.SFSampled
import SFML.Audio.SFSound
import SFML.Audio.SFSoundBuffer
import SFML.Audio.SoundStatus
import SFML.Audio.Types
import SFML.SFResource
import SFML.System.InputStream
import SFML.System.Time
import SFML.System.Vector3

import Control.Monad ((>=>))
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)


checkNull :: Music -> Maybe Music
checkNull music@(Music ptr) = if ptr == nullPtr then Nothing else Just music


-- | Create a new music and load it from a file.
-- 
-- This function doesn't start playing the music (call
-- sfMusic_play to do so).
-- 
-- Here is a complete list of all the supported audio formats:
-- ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
-- w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
musicFromFile :: FilePath -> IO (Maybe Music)
musicFromFile path = withCAString path $ \cstr -> sfMusic_createFromFile cstr >>= return . checkNull

foreign import ccall unsafe "sfMusic_createFromFile"
    sfMusic_createFromFile :: CString -> IO Music

-- \return A new sfMusic object (NULL if failed)

-- CSFML_AUDIO_API sfMusic* sfMusic_createFromFile(const char* filename);


-- | Create a new music and load it from a file in memory.
-- 
-- This function doesn't start playing the music (call
-- sfMusic_play to do so).
-- 
-- Here is a complete list of all the supported audio formats:
-- ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
-- w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
musicFromMemory
    :: Ptr a -- ^ Pointer to the file data in memory
    -> Int   -- ^ Size of the data to load, in bytes
    -> IO (Maybe Music)

musicFromMemory ptr size = fmap checkNull $ sfMusic_createFromMemory ptr (fromIntegral size)

foreign import ccall unsafe "sfMusic_createFromMemory"
    sfMusic_createFromMemory :: Ptr a -> CUInt -> IO Music

-- \return A new sfMusic object (NULL if failed)

-- CSFML_AUDIO_API sfMusic* sfMusic_createFromMemory(const void* data, size_t sizeInBytes);


-- | Create a new music and load it from a custom stream.
-- 
-- This function doesn't start playing the music (call
-- sfMusic_play to do so).
-- 
-- Here is a complete list of all the supported audio formats:
-- ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
-- w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
musicFromStream :: InputStream -> IO (Maybe Music)
musicFromStream is = with is $ \ptr -> sfMusic_createFromStream ptr >>= return . checkNull

foreign import ccall unsafe "sfMusic_createFromStream"
    sfMusic_createFromStream :: Ptr InputStream -> IO Music

-- \return A new sfMusic object (NULL if failed)

-- CSFML_AUDIO_API sfMusic* sfMusic_createFromStream(sfInputStream* stream);


instance SFResource Music where
    
    {-# INLINABLE destroy #-}
    destroy = sfMusic_destroy

foreign import ccall unsafe "sfMusic_destroy"
    sfMusic_destroy :: Music -> IO ()

-- CSFML_AUDIO_API void sfMusic_destroy(sfMusic* music);


instance SFSoundBuffer Music where
    
    {-# INLINABLE getChannelCount #-}
    getChannelCount = sfMusic_getChannelCount >=> return . fromIntegral
    
    {-# INLINABLE getDuration #-}
    getDuration music = alloca $ \ptr -> sfMusic_getDuration_helper music ptr >> peek ptr


foreign import ccall unsafe "sfMusic_getDuration_helper"
    sfMusic_getDuration_helper :: Music -> Ptr Time -> IO ()

-- CSFML_AUDIO_API sfTime sfMusic_getDuration(const sfMusic* music);

foreign import ccall unsafe "sfMusic_getChannelCount"
    sfMusic_getChannelCount :: Music -> IO CUInt

-- CSFML_AUDIO_API unsigned int sfMusic_getChannelCount(const sfMusic* music);


instance SFSampled Music where
    
    {-# INLINABLE getSampleRate #-}
    getSampleRate = sfMusic_getSampleRate


foreign import ccall unsafe "sfMusic_getSampleRate"
    sfMusic_getSampleRate :: Music -> IO Int

-- CSFML_AUDIO_API unsigned int sfMusic_getSampleRate(const sfMusic* music);


instance SFSound Music where
    
    {-# INLINABLE play #-}
    play = sfMusic_play
    
    {-# INLINABLE pause #-}
    pause = sfMusic_pause
    
    {-# INLINABLE stop #-}
    stop = sfMusic_stop
    
    {-# INLINABLE getAttenuation #-}
    getAttenuation = sfMusic_getAttenuation >=> return . realToFrac
    
    {-# INLINABLE getLoop #-}
    getLoop music = fmap (toEnum . fromIntegral) $ sfMusic_getLoop music
    
    {-# INLINABLE getMinDistance #-}
    getMinDistance = sfMusic_getMinDistance >=> return . realToFrac
    
    {-# INLINABLE getPitch #-}
    getPitch = sfMusic_getPitch >=> return . realToFrac
    
    {-# INLINABLE getPlayingOffset #-}
    getPlayingOffset music = alloca $ \ptr -> sfMusic_getPlayingOffset_helper music ptr >> peek ptr
    
    {-# INLINABLE getPosition #-}
    getPosition music = alloca $ \ptr -> sfMusic_getPosition_helper music ptr >> peek ptr
    
    {-# INLINABLE getStatus #-}
    getStatus = sfMusic_getStatus >=> return . toEnum . fromIntegral
    
    {-# INLINABLE getVolume #-}
    getVolume = sfMusic_getVolume >=> return . realToFrac
    
    {-# INLINABLE isRelativeToListener #-}
    isRelativeToListener = sfMusic_isRelativeToListener >=> return . toEnum . fromIntegral
    
    {-# INLINABLE setAttenuation #-}
    setAttenuation m a = sfMusic_setAttenuation m $ realToFrac a
    
    {-# INLINABLE setLoop #-}
    setLoop music val = sfMusic_setLoop music (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setMinDistance #-}
    setMinDistance m d = sfMusic_setMinDistance m $ realToFrac d
    
    {-# INLINABLE setPitch #-}
    setPitch m p = sfMusic_setPitch m $ realToFrac p
    
    {-# INLINABLE setPlayingOffset #-}
    setPlayingOffset = sfMusic_setPlayingOffset
    
    {-# INLINABLE setPosition #-}
    setPosition music pos = with pos $ sfMusic_setPosition_helper music
    
    {-# INLINABLE setRelativeToListener #-}
    setRelativeToListener music val = sfMusic_setRelativeToListener music (fromIntegral . fromEnum $ val)
    
    {-# INLINABLE setVolume #-}
    setVolume m v = sfMusic_setVolume m $ realToFrac v


foreign import ccall unsafe "sfMusic_play"
    sfMusic_play :: Music -> IO ()

-- CSFML_AUDIO_API void sfMusic_play(sfMusic* music);

foreign import ccall unsafe "sfMusic_pause"
    sfMusic_pause :: Music -> IO ()

-- CSFML_AUDIO_API void sfMusic_pause(sfMusic* music);

foreign import ccall unsafe "sfMusic_stop"
    sfMusic_stop :: Music -> IO ()

-- CSFML_AUDIO_API void sfMusic_stop(sfMusic* music);

foreign import ccall unsafe "sfMusic_getAttenuation"
    sfMusic_getAttenuation :: Music -> IO CFloat

-- CSFML_AUDIO_API float sfMusic_getAttenuation(const sfMusic* music);

foreign import ccall unsafe "sfMusic_getLoop"
    sfMusic_getLoop :: Music -> IO CInt

-- CSFML_AUDIO_API sfBool sfMusic_getLoop(const sfMusic* music);

foreign import ccall unsafe "sfMusic_getMinDistance"
    sfMusic_getMinDistance :: Music -> IO CFloat

-- CSFML_AUDIO_API float sfMusic_getMinDistance(const sfMusic* music);

foreign import ccall unsafe "sfMusic_getPitch"
    sfMusic_getPitch :: Music -> IO CFloat

-- CSFML_AUDIO_API float sfMusic_getPitch(const sfMusic* music);

foreign import ccall unsafe "sfMusic_getPlayingOffset_helper"
    sfMusic_getPlayingOffset_helper :: Music -> Ptr Time -> IO ()

-- CSFML_AUDIO_API sfTime sfMusic_getPlayingOffset(const sfMusic* music);

foreign import ccall unsafe "sfMusic_getPosition_helper"
    sfMusic_getPosition_helper :: Music -> Ptr Vec3f -> IO ()

-- CSFML_AUDIO_API sfVector3f sfMusic_getPosition(const sfMusic* music);

foreign import ccall unsafe "sfMusic_getStatus"
    sfMusic_getStatus :: Music -> IO CInt

-- CSFML_AUDIO_API sfSoundStatus sfMusic_getStatus(const sfMusic* music);

foreign import ccall unsafe "sfMusic_getVolume"
    sfMusic_getVolume :: Music -> IO CFloat

-- CSFML_AUDIO_API float sfMusic_getVolume(const sfMusic* music);

foreign import ccall unsafe "sfMusic_isRelativeToListener"
    sfMusic_isRelativeToListener :: Music -> IO CInt

-- CSFML_AUDIO_API sfBool sfMusic_isRelativeToListener(const sfMusic* music);

foreign import ccall unsafe "sfMusic_setAttenuation"
    sfMusic_setAttenuation :: Music -> CFloat -> IO ()

-- CSFML_AUDIO_API void sfMusic_setAttenuation(sfMusic* music, float attenuation);

foreign import ccall unsafe "sfMusic_setLoop"
    sfMusic_setLoop :: Music -> CInt -> IO ()

-- CSFML_AUDIO_API void sfMusic_setLoop(sfMusic* music, sfBool loop);

foreign import ccall unsafe "sfMusic_setMinDistance"
    sfMusic_setMinDistance :: Music -> CFloat -> IO ()

-- CSFML_AUDIO_API void sfMusic_setMinDistance(sfMusic* music, float distance);

foreign import ccall unsafe "sfMusic_setPitch"
    sfMusic_setPitch :: Music -> CFloat -> IO ()

-- CSFML_AUDIO_API void sfMusic_setPitch(sfMusic* music, float pitch);

foreign import ccall unsafe "sfMusic_setPlayingOffset"
    sfMusic_setPlayingOffset :: Music -> Time -> IO ()

-- CSFML_AUDIO_API void sfMusic_setPlayingOffset(sfMusic* music, sfTime timeOffset);

foreign import ccall unsafe "sfMusic_setPosition_helper"
    sfMusic_setPosition_helper :: Music -> Ptr Vec3f -> IO ()

-- CSFML_AUDIO_API void sfMusic_setPosition(sfMusic* music, sfVector3f position);

foreign import ccall unsafe "sfMusic_setRelativeToListener"
    sfMusic_setRelativeToListener :: Music -> CInt -> IO ()

-- CSFML_AUDIO_API void sfMusic_setRelativeToListener(sfMusic* music, sfBool relative);

foreign import ccall unsafe "sfMusic_setVolume"
    sfMusic_setVolume :: Music -> CFloat -> IO ()

-- CSFML_AUDIO_API void sfMusic_setVolume(sfMusic* music, float volume);

