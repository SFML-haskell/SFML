module SFML.Audio.SoundRecorder
(
    module SFML.Utils
,   SoundRecorderStartCallback
,   SoundRecorderProcessCallback
,   SoundRecorderStopCallback
,   createSoundRecorder
,   destroy
,   startRecording
,   stopRecording
,   getSampleRate
,   isSoundRecorderAvailable
,   setProcessingInterval
,   getAvailableSoundRecordingDevices
,   getDefaultSoundRecordingDevice
,   setSoundRecordingDevice
,   getSoundRecordingDevice
)
where


import SFML.Audio.SFSampled
import SFML.Audio.SFSoundRecorder
import SFML.Audio.Types
import SFML.SFException
import SFML.SFResource
import SFML.System.Time
import SFML.Utils

import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>=>), forM)
import Data.Word (Word16)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Storable
import Foreign.Ptr


checkNull :: SoundRecorder -> Maybe SoundRecorder
checkNull sr@(SoundRecorder ptr) = if ptr == nullPtr then Nothing else Just sr


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
    -> IO (Either SFException SoundRecorder) -- ^ A new 'SoundRecorder' object ('Nothing' if failed)

createSoundRecorder c1 c2 c3 d =
    let err = SFException $
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


-- | Set the processing interval.
--
-- The processing interval controls the period
-- between calls to the onProcessSamples function. You may
-- want to use a small interval if you want to process the
-- recorded data in real time, for example.
--
-- Note: this is only a hint, the actual period may vary.
-- So don't rely on this parameter to implement precise timing.
--
-- The default processing interval is 100 ms.
setProcessingInterval
    :: SoundRecorder
    -> Time -- ^ Processing interval
    -> IO ()

setProcessingInterval = sfSoundRecorder_setProcessingInterval

foreign import ccall unsafe "sfSoundRecorder_setProcessingInterval"
    sfSoundRecorder_setProcessingInterval :: SoundRecorder -> Time -> IO ()

--CSFML_AUDIO_API void sfSoundRecorder_setProcessingInterval(sfSoundRecorder* soundRecorder, sfTime interval);


-- | Get a list of the names of all availabe audio capture devices.
--
-- This function returns an array of strings (null terminated),
-- containing the names of all availabe audio capture devices.
-- If no devices are available then 'Nothing' is returned.
getAvailableSoundRecordingDevices :: IO [String]
getAvailableSoundRecordingDevices = alloca $ \pCount -> do
    pNames <- sfSoundRecorder_getAvailableDevices pCount
    count  <- fromIntegral <$> peek pCount :: IO Int
    forM [1..count] $ peekCString . advancePtr (castPtr pNames)

foreign import ccall unsafe "sfSoundRecorder_getAvailableDevices"
    sfSoundRecorder_getAvailableDevices :: Ptr CSize -> IO (Ptr CString)

--CSFML_AUDIO_API const char** sfSoundRecorder_getAvailableDevices(size_t* count);


-- | Get the name of the default audio capture device.
--
-- This function returns the name of the default audio
-- capture device. If none is available, NULL is returned.

getDefaultSoundRecordingDevice :: IO String
getDefaultSoundRecordingDevice = sfSoundRecorder_getDefaultDevice >>= peekCString

foreign import ccall unsafe "sfSoundRecorder_getDefaultDevice"
    sfSoundRecorder_getDefaultDevice :: IO CString

--CSFML_AUDIO_API const char* sfSoundRecorder_getDefaultDevice();


-- | Set the audio capture device.
--
-- This function sets the audio capture device to the device
-- with the given name. It can be called on the fly (i.e:
-- while recording). If you do so while recording and
-- opening the device fails, it stops the recording.
--
-- Return 'True if it was able to set the requested device, 'False' otherwise.
setSoundRecordingDevice
    :: SoundRecorder
    -> String -- ^ The name of the audio capture device
    -> IO Bool

setSoundRecordingDevice rec name = withCString name $ \cname ->
    ((/=0) . fromIntegral) <$> sfSoundRecorder_setDevice rec cname

foreign import ccall unsafe "sfSoundRecorder_setDevice"
    sfSoundRecorder_setDevice :: SoundRecorder -> CString -> IO CInt

--CSFML_AUDIO_API sfBool sfSoundRecorder_setDevice(sfSoundRecorder* soundRecorder, const char* name);


-- | Get the name of the current audio capture device.
getSoundRecordingDevice :: SoundRecorder -> IO String
getSoundRecordingDevice rec = sfSoundRecorder_getDevice rec >>= peekCString

foreign import ccall unsafe "sfSoundRecorder_getDevice"
    sfSoundRecorder_getDevice :: SoundRecorder -> IO CString

--CSFML_AUDIO_API const char* sfSoundRecorder_getDevice(sfSoundRecorder* soundRecorder);


