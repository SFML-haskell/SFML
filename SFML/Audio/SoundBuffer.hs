module SFML.Audio.SoundBuffer
(
    soundBufferFromFile
,   soundBufferFromMemory
,   soundBufferFromStream
,   soundBufferFromSamples
,   copySoundBuffer
,   destroySoundBuffer
,   saveSoundBufferToFile
,   getSamples
,   getSampleCount
,   getSampleRate
,   getChannelCount
,   getDuration
)
where


import SFML.Audio.SFSampled
import SFML.Audio.SFSoundBuffer
import SFML.Audio.Types
import SFML.System.InputStream
import SFML.System.Time

import Control.Monad ((>=>))
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Unsafe.Coerce (unsafeCoerce)


checkNull :: SoundBuffer -> Maybe SoundBuffer
checkNull buf@(SoundBuffer ptr) =
    case (unsafeCoerce ptr) of
        0 -> Nothing
        _ -> Just buf


-- | Create a new sound buffer and load it from a file.
--
-- Here is a complete list of all the supported audio formats:
-- ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
-- w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
soundBufferFromFile :: FilePath -> IO (Maybe SoundBuffer)
soundBufferFromFile path = fmap checkNull $ withCAString path sfSoundBuffer_createFromFile

foreign import ccall unsafe "sfSoundBuffer_createFromFile"
    sfSoundBuffer_createFromFile :: CString -> IO SoundBuffer

-- \return A new sfSoundBuffer object (NULL if failed)

-- CSFML_AUDIO_API sfSoundBuffer* sfSoundBuffer_createFromFile(const char* filename);


-- | Create a new sound buffer and load it from a file in memory.
--
-- Here is a complete list of all the supported audio formats:
-- ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
-- w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
soundBufferFromMemory
    :: Ptr a -- ^ Pointer to the file data in memory
    -> Int   -- ^ Size of the data to load, in bytes
    -> IO (Maybe SoundBuffer) -- ^ A new sfSoundBuffer object ('Nothing' if failed)

soundBufferFromMemory ptr size = sfSoundBuffer_createFromMemory ptr (fromIntegral size) >>= return . checkNull

foreign import ccall unsafe "sfSoundBuffer_createFromMemory"
    sfSoundBuffer_createFromMemory :: Ptr a -> CUInt -> IO SoundBuffer

-- CSFML_AUDIO_API sfSoundBuffer* sfSoundBuffer_createFromMemory(const void* data, size_t sizeInBytes);


-- | Create a new sound buffer and load it from a custom stream.
--
-- Here is a complete list of all the supported audio formats:
-- ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
-- w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
soundBufferFromStream
    :: InputStream
    -> IO (Maybe SoundBuffer) -- ^ A new sfSoundBuffer object ('Nothing' if failed)

soundBufferFromStream is = with is sfSoundBuffer_createFromStream >>= return . checkNull

foreign import ccall unsafe "sfSoundBuffer_createFromStream"
    sfSoundBuffer_createFromStream :: Ptr InputStream -> IO SoundBuffer

-- CSFML_AUDIO_API sfSoundBuffer* sfSoundBuffer_createFromStream(sfInputStream* stream);


-- | Create a new sound buffer and load it from an array of samples in memory.
--
-- The assumed format of the audio samples is 16 bits signed integer
-- (sfInt16).
soundBufferFromSamples
    :: Ptr a -- ^ Pointer to the array of samples in memory
    -> Int   -- ^ Number of samples in the array
    -> Int   -- ^ Number of channels (1 = mono, 2 = stereo, ...)
    -> Int   -- ^ Sample rate (number of samples to play per second)
    -> IO (Maybe SoundBuffer) -- ^ A new sfSoundBuffer object ('Nothing' if failed)

soundBufferFromSamples ptr i j k =
    sfSoundBuffer_createFromSamples ptr (fromIntegral i) (fromIntegral j) (fromIntegral k) >>= return . checkNull

foreign import ccall unsafe "sfSoundBuffer_createFromSamples"
    sfSoundBuffer_createFromSamples :: Ptr a -> CUInt -> CUInt -> CUInt -> IO SoundBuffer

-- CSFML_AUDIO_API sfSoundBuffer* sfSoundBuffer_createFromSamples(const sfInt16* samples, size_t sampleCount, unsigned int channelCount, unsigned int sampleRate);


-- | Create a new sound buffer by copying an existing one.
copySoundBuffer :: SoundBuffer -> IO SoundBuffer
copySoundBuffer = sfSoundBuffer_copy

foreign import ccall unsafe "sfSoundBuffer_copy"
    sfSoundBuffer_copy :: SoundBuffer -> IO SoundBuffer

-- CSFML_AUDIO_API sfSoundBuffer* sfSoundBuffer_copy(sfSoundBuffer* soundBuffer);


-- | Destroy a sound buffer
destroySoundBuffer :: SoundBuffer -> IO ()
destroySoundBuffer = sfSoundBuffer_destroy

foreign import ccall unsafe "sfSoundBuffer_destroy"
    sfSoundBuffer_destroy :: SoundBuffer -> IO ()

-- CSFML_AUDIO_API void sfSoundBuffer_destroy(sfSoundBuffer* soundBuffer);


-- | Save a sound buffer to an audio file.
--
-- Here is a complete list of all the supported audio formats:
-- ogg, wav, flac, aiff, au, raw, paf, svx, nist, voc, ircam,
-- w64, mat4, mat5 pvf, htk, sds, avr, sd2, caf, wve, mpc2k, rf64.
saveSoundBufferToFile
    :: SoundBuffer
    -> FilePath -- ^ Path of the sound file to write
    -> IO Bool  -- ^ 'True' if saving succeeded, 'False' if it failed

saveSoundBufferToFile sb path = fmap (toEnum . fromIntegral) . withCAString path $ sfSoundBuffer_saveToFile sb

foreign import ccall unsafe "sfSoundBuffer_saveToFile"
    sfSoundBuffer_saveToFile :: SoundBuffer -> CString -> IO CInt

-- CSFML_AUDIO_API sfBool sfSoundBuffer_saveToFile(const sfSoundBuffer* soundBuffer, const char* filename);


-- | Get the array of audio samples stored in a sound buffer.
--
-- The format of the returned samples is 16 bits signed integer
-- (sfInt16). The total number of samples in this array
-- is given by the sfSoundBuffer_getSampleCount function.
getSamples
    :: SoundBuffer
    -> IO (Ptr a) -- ^ Read-only pointer to the array of sound samples

getSamples = sfSoundBuffer_getSamples

foreign import ccall unsafe "sfSoundBuffer_getSamples"
    sfSoundBuffer_getSamples :: SoundBuffer -> IO (Ptr a)

-- CSFML_AUDIO_API const sfInt16* sfSoundBuffer_getSamples(const sfSoundBuffer* soundBuffer);


-- | Get the number of samples stored in a sound buffer.
--
-- The array of samples can be accessed with the
-- sfSoundBuffer_getSamples function.
getSampleCount :: SoundBuffer -> IO Int
getSampleCount = sfSoundBuffer_getSampleCount >=> return . fromIntegral

foreign import ccall unsafe "sfSoundBuffer_getSampleCount"
    sfSoundBuffer_getSampleCount :: SoundBuffer -> IO CUInt

-- CSFML_AUDIO_API size_t sfSoundBuffer_getSampleCount(const sfSoundBuffer* soundBuffer);


instance SFSoundBuffer SoundBuffer where
    
    getChannelCount = sfSoundBuffer_getChannelCount >=> return . fromIntegral
    
    getDuration sb = alloca $ \ptr -> sfSoundBuffer_getDuration_helper sb ptr >> peek ptr


foreign import ccall unsafe "sfSoundBuffer_getChannelCount"
    sfSoundBuffer_getChannelCount :: SoundBuffer -> IO CUInt

-- CSFML_AUDIO_API unsigned int sfSoundBuffer_getChannelCount(const sfSoundBuffer* soundBuffer);

foreign import ccall unsafe "sfSoundBuffer_getDuration_helper"
    sfSoundBuffer_getDuration_helper :: SoundBuffer -> Ptr Time -> IO ()

-- CSFML_AUDIO_API sfTime sfSoundBuffer_getDuration(const sfSoundBuffer* soundBuffer);


instance SFSampled SoundBuffer where
    
    getSampleRate = sfSoundBuffer_getSampleRate >=> return . fromIntegral


foreign import ccall unsafe "sfSoundBuffer_getSampleRate"
    sfSoundBuffer_getSampleRate :: SoundBuffer -> IO CUInt

-- CSFML_AUDIO_API unsigned int sfSoundBuffer_getSampleRate(const sfSoundBuffer* soundBuffer);

