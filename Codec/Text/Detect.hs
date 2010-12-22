module Codec.Text.Detect () where


-- typedef void* csd_t;
type Csd_t = Ptr ()

-- csd_t csd_open(void);
foreign import "csd_open" c_csd_open :: IO Csd_t

-- int csd_consider(csd_t csd, const char *data, int length);
foreign import "csd_consider" c_csd_consider :: Csd_t -> CString -> CInt -> IO CInt

-- const char *csd_close(csd_t csd);
foreign import "csd_close" c_csd_close :: Csd_t -> IO CString

