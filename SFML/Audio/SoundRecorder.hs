module SFML.Audio.SoundRecorder
(
    SoundRecorderStartCallback
,   SoundRecorderProcessCallback
,   SoundRecorderStopCallback
,   createSoundRecorder
,   destroySoundRecorder
,   startRecording
,   stopRecording
,   getSampleRate
,   isSoundRecorderAvailable
)
where


import SFML.Audio.SFSampled
import SFML.Audio.SFSoundRecorder
import SFML.Audio.Types

import Control.Monad ((>=>))
import Data.Word (Word16)
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Unsafe.Coerce (unsafeCoerce)


checkNull :: SoundRecorder -> Maybe SoundRecorder
checkNull sr@(SoundRecorder ptr) =
    case (unsafeCoerce ptr) of
        0 -> Nothing
        _ -> Just sr


type SoundRecorderStartCallback a = Ptr a -> IO CInt

type SoundRecorderProcessCallback a = Ptr Word16 -> CUInt -> Ptr a -> IO Bool

type SoundRecorderStopCallback a = Ptr a -> IO ()

-- Type of the callback used when starting a capture
-- typedef sfBool (*sfSoundRecorderStartCallback)(void*);

-- Type of the callback used to process audio data
-- typedef sfBool (*sfSoundRecorderProcessCallback)(const sfInt16*, size_t, void*);

-- Type of the callback used when stopping a capture
-- typedef void   (*sfSoundRecorderStopCallback)(void*);



-- | Construct a new sound recorder from callback functions.
createSoundRecorder
    :: Ptr (SoundRecorderStartCallback a)   -- ^ Callback function which will be called when a new capture starts (can be NULL)
    -> Ptr (SoundRecorderProcessCallback a) -- ^ Callback function which will be called each time there's audio data to process
    -> Ptr (SoundRecorderStopCallback a)    -- ^ Callback function which will be called when the current capture stops (can be NULL)
    -> Ptr a -- ^ Data to pass to the callback function (can be NULL)
    -> IO (Maybe SoundRecorder) -- ^ A new sfSoundRecorder object ('Nothing' if failed)

createSoundRecorder c1 c2 c3 d = sfSoundRecorder_create c1 c2 c3 d >>= return . checkNull

foreign import ccall unsafe "sfSoundRecorder_create"
    sfSoundRecorder_create
        :: Ptr (SoundRecorderStartCallback a)
        -> Ptr (SoundRecorderProcessCallback a)
        -> Ptr (SoundRecorderStopCallback a)
        -> Ptr a
        -> IO SoundRecorder

-- CSFML_AUDIO_API sfSoundRecorder* sfSoundRecorder_create(sfSoundRecorderStartCallback onStart, sfSoundRecorderProcessCallback onProcess, sfSoundRecorderStopCallback onStop, void* userData);


-- | Destroy a sound recorder.
destroySoundRecorder :: SoundRecorder -> IO ()
destroySoundRecorder = sfSoundRecorder_destroy

foreign import ccall unsafe "sfSoundRecorder_destroy"
    sfSoundRecorder_destroy :: SoundRecorder -> IO ()

-- CSFML_AUDIO_API void sfSoundRecorder_destroy(sfSoundRecorder* soundRecorder);


instance SFSoundRecorder SoundRecorder where
    
    startRecording rec rate = sfSoundRecorder_start rec (fromIntegral rate)
    
    stopRecording = sfSoundRecorder_stop


foreign import ccall unsafe "sfSoundRecorder_start"
    sfSoundRecorder_start :: SoundRecorder -> CUInt -> IO ()

-- CSFML_AUDIO_API void sfSoundRecorder_start(sfSoundRecorder* soundRecorder, unsigned int sampleRate);

foreign import ccall unsafe "sfSoundRecorder_stop"
    sfSoundRecorder_stop :: SoundRecorder -> IO ()

-- CSFML_AUDIO_API void sfSoundRecorder_stop(sfSoundRecorder* soundRecorder);


instance SFSampled SoundRecorder where
    
    getSampleRate = sfSoundRecorder_getSampleRate >=> return . fromIntegral


foreign import ccall unsafe "sfSoundRecorder_getSampleRate"
    sfSoundRecorder_getSampleRate :: SoundRecorder -> IO CUInt

-- CSFML_AUDIO_API unsigned int sfSoundRecorder_getSampleRate(const sfSoundRecorder* soundRecorder);


-- | Check if the system supports audio capture.
--
-- This function should always be called before using
-- the audio capture features. If it returns false, then
-- any attempt to use sfSoundRecorder will fail.
isSoundRecorderAvailable :: IO Bool
isSoundRecorderAvailable = fmap (toEnum . fromIntegral) sfSoundRecorder_isAvailable

foreign import ccall unsafe "sfSoundRecorder_isAvailable"
    sfSoundRecorder_isAvailable :: IO CInt

-- CSFML_AUDIO_API sfBool sfSoundRecorder_isAvailable(void);

