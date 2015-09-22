{-# LANGUAGE DeriveDataTypeable #-}
module SFML.Audio.SoundRecorder
(
    module SFML.Utils
,   SoundRecorderException(..)
,   SoundRecorderStartCallback
,   SoundRecorderProcessCallback
,   SoundRecorderStopCallback
,   createSoundRecorder
,   destroy
,   startRecording
,   stopRecording
,   getSampleRate
,   isSoundRecorderAvailable
)
where


import SFML.Audio.SFSampled
import SFML.Audio.SFSoundRecorder
import SFML.Audio.Types
import SFML.SFResource
import SFML.Utils

import Control.Exception
import Control.Monad ((>=>))
import Data.Typeable
import Data.Word (Word16)
import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr)


checkNull :: SoundRecorder -> Maybe SoundRecorder
checkNull sr@(SoundRecorder ptr) = if ptr == nullPtr then Nothing else Just sr


data SoundRecorderException = SoundRecorderException String deriving (Show, Typeable)

instance Exception SoundRecorderException


-- | Type of the callback used when starting a capture.
type SoundRecorderStartCallback a = Ptr a -> IO CInt

-- | Type of the callback used to process audio data.
type SoundRecorderProcessCallback a = Ptr Word16 -> CUInt -> Ptr a -> IO Bool

-- | Type of the callback used when stopping a capture.
type SoundRecorderStopCallback a = Ptr a -> IO ()

-- typedef sfBool (*sfSoundRecorderStartCallback)(void*);

-- typedef sfBool (*sfSoundRecorderProcessCallback)(const sfInt16*, size_t, void*);

-- typedef void   (*sfSoundRecorderStopCallback)(void*);



-- | Construct a new sound recorder from callback functions.
createSoundRecorder
    :: Ptr (SoundRecorderStartCallback a)   -- ^ (onStart) Callback function which will be called when a new capture starts (can be NULL)
    -> Ptr (SoundRecorderProcessCallback a) -- ^ (onProcess) Callback function which will be called each time there's audio data to process
    -> Ptr (SoundRecorderStopCallback a)    -- ^ (onStop) Callback function which will be called when the current capture stops (can be NULL)
    -> Ptr a -- ^ Data to pass to the callback function (can be NULL)
    -> IO (Either SoundRecorderException SoundRecorder) -- ^ A new 'SoundRecorder' object ('Nothing' if failed)

createSoundRecorder c1 c2 c3 d =
    let err = SoundRecorderException $
            "Failed creating sound recorder: onStart = " ++ show c1 ++
                                           " onProcess = " ++ show c2 ++
                                           " onStop = " ++ show c3 ++
                                           " userData = " ++ show d
    in sfSoundRecorder_create c1 c2 c3 d >>= return . tagErr err . checkNull

foreign import ccall unsafe "sfSoundRecorder_create"
    sfSoundRecorder_create
        :: Ptr (SoundRecorderStartCallback a)
        -> Ptr (SoundRecorderProcessCallback a)
        -> Ptr (SoundRecorderStopCallback a)
        -> Ptr a
        -> IO SoundRecorder

-- CSFML_AUDIO_API sfSoundRecorder* sfSoundRecorder_create(sfSoundRecorderStartCallback onStart, sfSoundRecorderProcessCallback onProcess, sfSoundRecorderStopCallback onStop, void* userData);


instance SFResource SoundRecorder where

    {-# INLINABLE destroy #-}
    destroy = sfSoundRecorder_destroy

foreign import ccall unsafe "sfSoundRecorder_destroy"
    sfSoundRecorder_destroy :: SoundRecorder -> IO ()

-- CSFML_AUDIO_API void sfSoundRecorder_destroy(sfSoundRecorder* soundRecorder);


instance SFSoundRecorder SoundRecorder where

    {-# INLINABLE startRecording #-}
    startRecording rec rate = ((/=0) . fromIntegral) <$> sfSoundRecorder_start rec (fromIntegral rate)

    {-# INLINABLE stopRecording #-}
    stopRecording = sfSoundRecorder_stop


foreign import ccall unsafe "sfSoundRecorder_start"
    sfSoundRecorder_start :: SoundRecorder -> CUInt -> IO CInt

-- CSFML_AUDIO_API void sfSoundRecorder_start(sfSoundRecorder* soundRecorder, unsigned int sampleRate);

foreign import ccall unsafe "sfSoundRecorder_stop"
    sfSoundRecorder_stop :: SoundRecorder -> IO ()

-- CSFML_AUDIO_API void sfSoundRecorder_stop(sfSoundRecorder* soundRecorder);


instance SFSampled SoundRecorder where

    {-# INLINABLE getSampleRate #-}
    getSampleRate = sfSoundRecorder_getSampleRate >=> return . fromIntegral


foreign import ccall unsafe "sfSoundRecorder_getSampleRate"
    sfSoundRecorder_getSampleRate :: SoundRecorder -> IO CUInt

-- CSFML_AUDIO_API unsigned int sfSoundRecorder_getSampleRate(const sfSoundRecorder* soundRecorder);


-- | Check if the system supports audio capture.
--
-- This function should always be called before using
-- the audio capture features. If it returns false, then
-- any attempt to use 'SoundRecorder' will fail.
isSoundRecorderAvailable :: IO Bool
isSoundRecorderAvailable = fmap (toEnum . fromIntegral) sfSoundRecorder_isAvailable

foreign import ccall unsafe "sfSoundRecorder_isAvailable"
    sfSoundRecorder_isAvailable :: IO CInt

-- CSFML_AUDIO_API sfBool sfSoundRecorder_isAvailable(void);

