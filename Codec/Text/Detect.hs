{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-- | Detect the likely character encoding for a stream of bytes using Mozilla's Universal Character Set Detector.
module Codec.Text.Detect (detectEncodingName, detectEncoding) where

import Control.Exception

import qualified Data.ByteString.Internal as SI
import qualified Data.ByteString.Lazy as L
import Data.Traversable (traverse)
import Control.Applicative

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO
import System.IO.Unsafe


#if !MIN_VERSION_base(4,3,0)
mask :: ((IO a -> IO a) -> IO b) -> IO b
mask io = blocked >>= \b -> if b then io id else block $ io unblock
#endif


-- typedef void* csd_t;
type Csd_t = Ptr ()

-- csd_t csd_open(void);
foreign import ccall unsafe "csd_open" c_csd_open :: IO Csd_t

-- int csd_consider(csd_t csd, const char *data, int length);
foreign import ccall unsafe "csd_consider" c_csd_consider :: Csd_t -> CString -> CInt -> IO CInt

-- const char *csd_close(csd_t csd);
foreign import ccall unsafe "csd_close" c_csd_close :: Csd_t -> IO CString


-- | Detect the likely encoding used by a 'L.ByteString'. At the time of writing, the encoding
-- returned will be drawn from this list:
--
-- > Big5
-- > EUC-JP
-- > EUC-KR
-- > gb18030
-- > HZ-GB-2312
-- > IBM855
-- > IBM866
-- > ISO-2022-CN
-- > ISO-2022-JP
-- > ISO-2022-KR
-- > ISO-8859-2
-- > ISO-8859-5
-- > ISO-8859-7
-- > ISO-8859-8
-- > KOI8-R
-- > Shift_JIS
-- > TIS-620
-- > UTF-8
-- > UTF-16BE
-- > UTF-16LE
-- > UTF-32BE
-- > UTF-32LE
-- > windows-1250
-- > windows-1251
-- > windows-1252
-- > windows-1253
-- > windows-1255
-- > x-euc-tw
-- > X-ISO-10646-UCS-4-2143
-- > X-ISO-10646-UCS-4-3412
-- > x-mac-cyrillic
{-# NOINLINE detectEncodingName #-}
detectEncodingName :: L.ByteString -> Maybe String
detectEncodingName b = unsafePerformIO $ do
    mask $ \restore -> do
        csd <- c_csd_open
        restore ((\f -> foldr f (return ()) (L.toChunks b)) $ \chunk feed_more -> do
            let (fptr, ptr_offset, chunk_length) = SI.toForeignPtr chunk
            res <- withForeignPtr fptr $ \ptr -> c_csd_consider csd (ptr `plusPtr` ptr_offset) (fromIntegral chunk_length)
            case res `compare` 0 of
              LT -> return () -- Some sort of error: could report it?
              EQ -> feed_more -- Feed more data to come to a conclusion
              GT -> return () -- We have enough data!
          ) `onException` c_csd_close csd
        c_encoding_ptr <- c_csd_close csd
        if c_encoding_ptr == nullPtr
         then return Nothing
         else Just . normalise <$> peekCString c_encoding_ptr
  where
    normalise "GB18030" = "gb18030"
    normalise x         = x

-- | Detect the encoding for a 'L.ByteString' and attempt to create a 'TextEncoding' suitable for decoding it.
detectEncoding :: L.ByteString -> IO (Maybe TextEncoding)
detectEncoding = traverse mkTextEncoding . detectEncodingName
