{-# LANGUAGE CPP, ForeignFunctionInterface, ExistentialQuantification #-}
module SFML.System.InputStream
(
    InputStreamReadFunc
,   InputStreamSeekFunc
,   InputStreamTellFunc
,   InputStreamGetSizeFunc
,   InputStream(..)
)
where


import Data.Word (Word64)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable


#include <SFML/System/InputStream.h>


-- | Function to read data from the stream.
type InputStreamReadFunc a
    =  Ptr Char  -- ^ Buffer where to copy the read data
    -> Word64    -- ^ Desired number of bytes to read
    -> Ptr a     -- ^ User data
    -> IO Word64 -- ^ The number of bytes actually read

-- | Function to set the current read position.
type InputStreamSeekFunc a
    =  Word64    -- ^ The position to seek to, from the beginning
    -> Ptr a     -- ^ User data
    -> IO Word64 -- ^ The position actually sought to, or -1 on error

-- | Function to get the current read position.
type InputStreamTellFunc a
    =  Ptr a     -- ^ User data
    -> IO Word64 -- ^ The current position, or -1 on error

-- | Function to get the total number of bytes in the stream.
type InputStreamGetSizeFunc a
    =  Ptr a     -- ^ User data
    -> IO Word64 -- ^ The total number of bytes available in the stream, or -1 on error


-- | Set of callbacks that allow users to define custom file streams.
data InputStream = forall a. InputStream
    { read       :: Ptr (InputStreamReadFunc a)
    , seek       :: Ptr (InputStreamSeekFunc a)
    , tell       :: Ptr (InputStreamTellFunc a)
    , getSize    :: Ptr (InputStreamGetSizeFunc a)
    , userData   :: Ptr a
    }

instance Storable InputStream where
    sizeOf _ = size_InputStream
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = do
        read  <- #{peek sfInputStream, read} ptr
        seek  <- #{peek sfInputStream, seek} ptr
        tell  <- #{peek sfInputStream, tell} ptr
        gets  <- #{peek sfInputStream, getSize} ptr
        udata <- #{peek sfInputStream, userData} ptr
        return $ InputStream read seek tell gets udata
    
    poke ptr (InputStream read seek tell gets udata) = do
        #{poke sfInputStream, read} ptr read
        #{poke sfInputStream, seek} ptr seek
        #{poke sfInputStream, tell} ptr tell
        #{poke sfInputStream, getSize} ptr gets
        #{poke sfInputStream, userData} ptr udata


size_InputStream = #{size sfInputStream}


instance Show InputStream where
    
    show (InputStream read seek tell getSize userData) =
        "InputStream { read = " ++ show read ++
                    ", seek = " ++ show seek ++
                    ", tell = " ++ show tell ++
                    ", getSize = " ++ show getSize ++
                    ", userData = " ++ show userData ++
                    "}"

