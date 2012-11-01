module SFML.Audio.Music
(
)
where


import SFML.Audio.SoundStatus
import SFML.Audio.Types
import SFML.System.InputStream
import SFML.System.Time
import SFML.System.Vector3

import Control.Monad ((>=>))
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Unsafe.Coerce (unsafeCoerce)


checkNull :: Music -> Maybe Music
checkNull music@(Music ptr) =
    case (unsafeCoerce ptr) of
        0 -> Nothing
        _ -> Just music


--  | Create a new music and load it from a file.
-- 
--  This function doesn't start playing the music (call
--  sfMusic_play to do so).
--  Here is a complete list of all the supported audio formats:
--  ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
--  w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
musicFromFile :: FilePath -> IO (Maybe Music)
musicFromFile path = withCAString path $ \cstr -> sfMusic_createFromFile cstr >>= return . checkNull

foreign import ccall unsafe "sfMusic_createFromFile"
    sfMusic_createFromFile :: CString -> IO Music

--  \return A new sfMusic object (NULL if failed)

-- CSFML_AUDIO_API sfMusic* sfMusic_createFromFile(const char* filename);


--  | Create a new music and load it from a file in memory.
-- 
--  This function doesn't start playing the music (call
--  sfMusic_play to do so).
--  Here is a complete list of all the supported audio formats:
--  ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
--  w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
musicFromMemory
    :: Ptr a -- ^ Pointer to the file data in memory
    -> Int   -- ^ Size of the data to load, in bytes
    -> IO (Maybe Music)

musicFromMemory ptr size = fmap checkNull $ sfMusic_createFromMemory ptr (fromIntegral size)

foreign import ccall unsafe "sfMusic_createFromMemory"
    sfMusic_createFromMemory :: Ptr a -> CUInt -> IO Music

--  \return A new sfMusic object (NULL if failed)

-- CSFML_AUDIO_API sfMusic* sfMusic_createFromMemory(const void* data, size_t sizeInBytes);


--  | Create a new music and load it from a custom stream.
-- 
--  This function doesn't start playing the music (call
--  sfMusic_play to do so).
--  Here is a complete list of all the supported audio formats:
--  ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
--  w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
musicFromStream :: InputStream -> IO (Maybe Music)
musicFromStream is = with is $ \ptr -> sfMusic_createFromStream ptr >>= return . checkNull

foreign import ccall unsafe "sfMusic_createFromStream"
    sfMusic_createFromStream :: Ptr InputStream -> IO Music

--  \return A new sfMusic object (NULL if failed)

-- CSFML_AUDIO_API sfMusic* sfMusic_createFromStream(sfInputStream* stream);


--  | Destroy a music.
destroyMusic :: Music -> IO ()
destroyMusic = sfMusic_destroy

foreign import ccall unsafe "sfMusic_destroy"
    sfMusic_destroy :: Music -> IO ()

-- CSFML_AUDIO_API void sfMusic_destroy(sfMusic* music);


--  | Set whether or not a music should loop after reaching the end.
-- 
--  If set, the music will restart from beginning after
--  reaching the end and so on, until it is stopped or
--  sfMusic_setLoop(music, sfFalse) is called.
--  The default looping state for musics is false.
setLoop :: Music -> Bool -> IO ()
setLoop music val = sfMusic_setLoop music (fromIntegral . fromEnum $ val)

foreign import ccall unsafe "sfMusic_setLoop"
    sfMusic_setLoop :: Music -> CInt -> IO ()

-- CSFML_AUDIO_API void sfMusic_setLoop(sfMusic* music, sfBool loop);


--  | Tell whether or not a music is in loop mode.
getLoop :: Music -> IO Bool
getLoop music = fmap (toEnum . fromIntegral) $ sfMusic_getLoop music

foreign import ccall unsafe "sfMusic_getLoop"
    sfMusic_getLoop :: Music -> IO CInt

-- CSFML_AUDIO_API sfBool sfMusic_getLoop(const sfMusic* music);


--  | Get the total duration of a music.
getDuration :: Music -> IO Time
getDuration music = alloca $ \ptr -> sfMusic_getDuration_helper music ptr >> peek ptr

foreign import ccall unsafe "sfMusic_getDuration_helper"
    sfMusic_getDuration_helper :: Music -> Ptr Time -> IO ()

-- CSFML_AUDIO_API sfTime sfMusic_getDuration(const sfMusic* music);


--  | Start or resume playing a music.
-- 
--  This function starts the music if it was stopped, resumes
--  it if it was paused, and restarts it from beginning if it
--  was it already playing.
--  This function uses its own thread so that it doesn't block
--  the rest of the program while the music is played.
play :: Music -> IO ()
play = sfMusic_play

foreign import ccall unsafe "sfMusic_play"
    sfMusic_play :: Music -> IO ()

-- CSFML_AUDIO_API void sfMusic_play(sfMusic* music);


--  | Pause a music.
-- 
--  This function pauses the music if it was playing,
--  otherwise (music already paused or stopped) it has no effect.
pause :: Music -> IO ()
pause = sfMusic_pause

foreign import ccall unsafe "sfMusic_pause"
    sfMusic_pause :: Music -> IO ()

-- CSFML_AUDIO_API void sfMusic_pause(sfMusic* music);


--  | Stop playing a music.
-- 
--  This function stops the music if it was playing or paused,
--  and does nothing if it was already stopped.
--  It also resets the playing position (unlike sfMusic_pause).
stop :: Music -> IO ()
stop = sfMusic_stop

foreign import ccall unsafe "sfMusic_stop"
    sfMusic_stop :: Music -> IO ()

-- CSFML_AUDIO_API void sfMusic_stop(sfMusic* music);


--  | Return the number of channels of a music.
-- 
--  1 channel means a mono sound, 2 means stereo, etc.
getChannelCount :: Music -> IO Int
getChannelCount = sfMusic_getChannelCount >=> return . fromIntegral

foreign import ccall unsafe "sfMusic_getChannelCount"
    sfMusic_getChannelCount :: Music -> IO CUInt

-- CSFML_AUDIO_API unsigned int sfMusic_getChannelCount(const sfMusic* music);


--  | Get the sample rate of a music.
-- 
--  The sample rate is the number of audio samples played per
--  second. The higher, the better the quality.
getSampleRate
    :: Music
    -> IO Int -- ^ Sample rate, in number of samples per second

getSampleRate = sfMusic_getSampleRate

foreign import ccall unsafe "sfMusic_getSampleRate"
    sfMusic_getSampleRate :: Music -> IO Int

-- CSFML_AUDIO_API unsigned int sfMusic_getSampleRate(const sfMusic* music);


--  | Get the current status of a music (stopped, paused, playing).
getStatus :: Music -> IO SoundStatus
getStatus = sfMusic_getStatus >=> return . toEnum

foreign import ccall unsafe "sfMusic_getStatus"
    sfMusic_getStatus :: Music -> IO Int

-- CSFML_AUDIO_API sfSoundStatus sfMusic_getStatus(const sfMusic* music);


--  | Get the current playing position of a music.
getPlayingOffset :: Music -> IO Time
getPlayingOffset music = alloca $ \ptr -> sfMusic_getPlayingOffset_helper music ptr >> peek ptr

foreign import ccall unsafe "sfMusic_getPlayingOffset_helper"
    sfMusic_getPlayingOffset_helper :: Music -> Ptr Time -> IO ()

-- CSFML_AUDIO_API sfTime sfMusic_getPlayingOffset(const sfMusic* music);


--  | Set the pitch of a music.
-- 
--  The pitch represents the perceived fundamental frequency
--  of a sound; thus you can make a music more acute or grave
--  by changing its pitch. A side effect of changing the pitch
--  is to modify the playing speed of the music as well.
--  The default value for the pitch is 1.
setPitch :: Music -> Float -> IO ()
setPitch = sfMusic_setPitch

foreign import ccall unsafe "sfMusic_setPitch"
    sfMusic_setPitch :: Music -> Float -> IO ()

-- CSFML_AUDIO_API void sfMusic_setPitch(sfMusic* music, float pitch);


--  | Set the volume of a music.
-- 
--  The volume is a value between 0 (mute) and 100 (full volume).
--  The default value for the volume is 100.
setVolume :: Music -> Float -> IO ()
setVolume = sfMusic_setVolume

foreign import ccall unsafe "sfMusic_setVolume"
    sfMusic_setVolume :: Music -> Float -> IO ()

-- CSFML_AUDIO_API void sfMusic_setVolume(sfMusic* music, float volume);


--  | Set the 3D position of a music in the audio scene.
-- 
--  Only musics with one channel (mono musics) can be
--  spatialized.
--  The default position of a music is (0, 0, 0).
setPosition :: Music -> Vec3f -> IO ()
setPosition music pos = with pos $ sfMusic_setPosition_helper music

foreign import ccall unsafe "sfMusic_setPosition_helper"
    sfMusic_setPosition_helper :: Music -> Ptr Vec3f -> IO ()

-- CSFML_AUDIO_API void sfMusic_setPosition(sfMusic* music, sfVector3f position);


--  | Make a musics's position relative to the listener or absolute.
-- 
--  Making a music relative to the listener will ensure that it will always
--  be played the same way regardless the position of the listener.
--  This can be useful for non-spatialized musics, musics that are
--  produced by the listener, or musics attached to it.
--  The default value is false (position is absolute).
setRelativeToListener :: Music -> Bool -> IO ()
setRelativeToListener music val = sfMusic_setRelativeToListener music (fromEnum val)

foreign import ccall unsafe "sfMusic_setRelativeToListener"
    sfMusic_setRelativeToListener :: Music -> Int -> IO ()

-- CSFML_AUDIO_API void sfMusic_setRelativeToListener(sfMusic* music, sfBool relative);


--  | Set the minimum distance of a music.
-- 
--  The "minimum distance" of a music is the maximum
--  distance at which it is heard at its maximum volume. Further
--  than the minimum distance, it will start to fade out according
--  to its attenuation factor. A value of 0 ("inside the head
--  of the listener") is an invalid value and is forbidden.
--  The default value of the minimum distance is 1.
setMinDistance :: Music -> Float -> IO ()
setMinDistance = sfMusic_setMinDistance

foreign import ccall unsafe "sfMusic_setMinDistance"
    sfMusic_setMinDistance :: Music -> Float -> IO ()

-- CSFML_AUDIO_API void sfMusic_setMinDistance(sfMusic* music, float distance);


--  | Set the attenuation factor of a music.
-- 
--  The attenuation is a multiplicative factor which makes
--  the music more or less loud according to its distance
--  from the listener. An attenuation of 0 will produce a
--  non-attenuated music, i.e. its volume will always be the same
--  whether it is heard from near or from far. On the other hand,
--  an attenuation value such as 100 will make the music fade out
--  very quickly as it gets further from the listener.
--  The default value of the attenuation is 1.
setAttenuation :: Music -> Float -> IO ()
setAttenuation = sfMusic_setAttenuation

foreign import ccall unsafe "sfMusic_setAttenuation"
    sfMusic_setAttenuation :: Music -> Float -> IO ()

-- CSFML_AUDIO_API void sfMusic_setAttenuation(sfMusic* music, float attenuation);


--  | Change the current playing position of a music.
-- 
--  The playing position can be changed when the music is
--  either paused or playing.
setPlayingOffset :: Music -> Time -> IO ()
setPlayingOffset = sfMusic_setPlayingOffset

foreign import ccall unsafe "sfMusic_setPlayingOffset"
    sfMusic_setPlayingOffset :: Music -> Time -> IO ()

-- CSFML_AUDIO_API void sfMusic_setPlayingOffset(sfMusic* music, sfTime timeOffset);


--  | Get the pitch of a music.
getPitch :: Music -> IO Float
getPitch = sfMusic_getPitch

foreign import ccall unsafe "sfMusic_getPitch"
    sfMusic_getPitch :: Music -> IO Float

-- CSFML_AUDIO_API float sfMusic_getPitch(const sfMusic* music);


--  | Get the volume of a music.
getVolume :: Music -> IO Float
getVolume = sfMusic_getVolume

foreign import ccall unsafe "sfMusic_getVolume"
    sfMusic_getVolume :: Music -> IO Float

-- CSFML_AUDIO_API float sfMusic_getVolume(const sfMusic* music);


--  | Get the 3D position of a music in the audio scene.
getPosition :: Music -> IO Vec3f
getPosition music = alloca $ \ptr -> sfMusic_getPosition_helper music ptr >> peek ptr

foreign import ccall unsafe "sfMusic_getPosition_helper"
    sfMusic_getPosition_helper :: Music -> Ptr Vec3f -> IO ()

-- CSFML_AUDIO_API sfVector3f sfMusic_getPosition(const sfMusic* music);


--  | Tell whether a music's position is relative to the listener or if it is absolute.
isRelativeToListener :: Music -> IO Bool
isRelativeToListener = sfMusic_isRelativeToListener >=> return . toEnum

foreign import ccall unsafe "sfMusic_isRelativeToListener"
    sfMusic_isRelativeToListener :: Music -> IO Int

-- CSFML_AUDIO_API sfBool sfMusic_isRelativeToListener(const sfMusic* music);


--  | Get the minimum distance of a music
getMinDistance :: Music -> IO Float
getMinDistance = sfMusic_getMinDistance

foreign import ccall unsafe "sfMusic_getMinDistance"
    sfMusic_getMinDistance :: Music -> IO Float

-- CSFML_AUDIO_API float sfMusic_getMinDistance(const sfMusic* music);


--  | Get the attenuation factor of a music
getAttenuation :: Music -> IO Float
getAttenuation = sfMusic_getAttenuation

foreign import ccall unsafe "sfMusic_getAttenuation"
    sfMusic_getAttenuation :: Music -> IO Float

-- CSFML_AUDIO_API float sfMusic_getAttenuation(const sfMusic* music);

