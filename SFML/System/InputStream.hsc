{-# LANGUAGE CPP, ForeignFunctionInterface, ExistentialQuantification #-}
module SFML.System.InputStream
(
    SFInputStreamReadFunc
,   SFInputStreamSeekFunc
,   SFInputStreamTellFunc
,   SFInputStreamGetSizeFunc
,   SFInputStream(..)
)
where


import Data.Word (Word64)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable


#include <SFML/System/InputStream.h>


-- | Function to read data from the stream.
type SFInputStreamReadFunc a
    =  Ptr Char  -- ^ Buffer where to copy the read data
    -> Word64    -- ^ Desired number of bytes to read
    -> Ptr a     -- ^ User data
    -> IO Word64 -- ^ The number of bytes actually read

-- | Function to set the current read position.
type SFInputStreamSeekFunc a
    =  Word64    -- ^ The position to seek to, from the beginning
    -> Ptr a     -- ^ User data
    -> IO Word64 -- ^ The position actually sought to, or -1 on error

-- | Function to get the current read position.
type SFInputStreamTellFunc a
    =  Ptr a     -- ^ User data
    -> IO Word64 -- ^ The current position, or -1 on error

-- | Function to get the total number of bytes in the stream.
type SFInputStreamGetSizeFunc a
    =  Ptr a     -- ^ User data
    -> IO Word64 -- ^ The total number of bytes available in the stream, or -1 on error


-- | Set of callbacks that allow users to define custom file streams.
data SFInputStream = forall a. SFInputStream
    { read       :: Ptr (SFInputStreamReadFunc a)
    , seek       :: Ptr (SFInputStreamSeekFunc a)
    , tell       :: Ptr (SFInputStreamTellFunc a)
    , getSize    :: Ptr (SFInputStreamGetSizeFunc a)
    , userData   :: Ptr a
    }

instance Storable SFInputStream where
    sizeOf _ = size_InputStream
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = do
        read  <- #{peek sfInputStream, read} ptr
        seek  <- #{peek sfInputStream, seek} ptr
        tell  <- #{peek sfInputStream, tell} ptr
        gets  <- #{peek sfInputStream, getSize} ptr
        udata <- #{peek sfInputStream, userData} ptr
        return $ SFInputStream read seek tell gets udata
    
    poke ptr (SFInputStream read seek tell gets udata) = do
        #{poke sfInputStream, read} ptr read
        #{poke sfInputStream, seek} ptr seek
        #{poke sfInputStream, tell} ptr tell
        #{poke sfInputStream, getSize} ptr gets
        #{poke sfInputStream, userData} ptr udata


size_InputStream = #{size sfInputStream}

