module SFML.Audio.SoundBufferRecorder
(
    createSoundBufferRecorder
,   destroySoundBufferRecorder
,   startRecording
,   stopRecording
,   getSampleRate
,   getRecorderBuffer
)
where


import SFML.Audio.SFSampled
import SFML.Audio.Types

import Control.Monad ((>=>))
import Foreign.C.Types
import Unsafe.Coerce (unsafeCoerce)


checkNull :: SoundBufferRecorder -> Maybe SoundBufferRecorder
checkNull sbr@(SoundBufferRecorder ptr) =
    case (unsafeCoerce ptr) of
        0 -> Nothing
        _ -> Just sbr


-- | Create a new sound buffer recorder.
createSoundBufferRecorder :: IO (Maybe SoundBufferRecorder)
createSoundBufferRecorder = fmap checkNull sfSoundBufferRecorder_create

foreign import ccall unsafe "sfSoundBufferRecorder_create"
    sfSoundBufferRecorder_create :: IO SoundBufferRecorder

-- \return A new sfSoundBufferRecorder object (NULL if failed)

-- CSFML_AUDIO_API sfSoundBufferRecorder* sfSoundBufferRecorder_create(void);


-- | Destroy a sound buffer recorder.
destroySoundBufferRecorder :: SoundBufferRecorder -> IO ()
destroySoundBufferRecorder = sfSoundBufferRecorder_destroy

foreign import ccall unsafe "sfSoundBufferRecorder_destroy"
    sfSoundBufferRecorder_destroy :: SoundBufferRecorder -> IO ()

-- CSFML_AUDIO_API void sfSoundBufferRecorder_destroy(sfSoundBufferRecorder* soundBufferRecorder);


-- | Start the capture of a sound recorder recorder.
--
-- The sampleRate parameter defines the number of audio samples
-- captured per second. The higher, the better the quality
-- (for example, 44100 samples/sec is CD quality).
--
-- This function uses its own thread so that it doesn't block
-- the rest of the program while the capture runs.
-- Please note that only one capture can happen at the same time.
startRecording
    :: SoundBufferRecorder
    -> Int -- ^ Desired capture rate, in number of samples per second
    -> IO ()

startRecording sbr sr = sfSoundBufferRecorder_start sbr (fromIntegral sr)

foreign import ccall unsafe "sfSoundBufferRecorder_start"
    sfSoundBufferRecorder_start :: SoundBufferRecorder -> CUInt -> IO ()

-- CSFML_AUDIO_API void sfSoundBufferRecorder_start(sfSoundBufferRecorder* soundBufferRecorder, unsigned int sampleRate);


-- | Stop the capture of a sound recorder.
stopRecording :: SoundBufferRecorder -> IO ()
stopRecording = sfSoundBufferRecorder_stop

foreign import ccall unsafe "sfSoundBufferRecorder_stop"
    sfSoundBufferRecorder_stop :: SoundBufferRecorder -> IO ()

-- CSFML_AUDIO_API void sfSoundBufferRecorder_stop(sfSoundBufferRecorder* soundBufferRecorder);


instance SFSampled SoundBufferRecorder where
    
    getSampleRate = sfSoundBufferRecorder_getSampleRate >=> return . fromIntegral


foreign import ccall unsafe "sfSoundBufferRecorder_getSampleRate"
    sfSoundBufferRecorder_getSampleRate :: SoundBufferRecorder -> IO CUInt

-- CSFML_AUDIO_API unsigned int sfSoundBufferRecorder_getSampleRate(const sfSoundBufferRecorder* soundBufferRecorder);


-- | Get the sound buffer containing the captured audio data.
--
-- The sound buffer is valid only after the capture has ended.
-- This function provides a read-only access to the internal
-- sound buffer, but it can be copied if you need to
-- make any modification to it.
getRecorderBuffer
    :: SoundBufferRecorder
    -> IO SoundBuffer -- ^ Read-only access to the sound buffer

getRecorderBuffer = sfSoundBufferRecorder_getBuffer

foreign import ccall unsafe "sfSoundBufferRecorder_getBuffer"
    sfSoundBufferRecorder_getBuffer :: SoundBufferRecorder -> IO SoundBuffer

-- CSFML_AUDIO_API const sfSoundBuffer* sfSoundBufferRecorder_getBuffer(const sfSoundBufferRecorder* soundBufferRecorder);

