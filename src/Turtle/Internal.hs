module Turtle.Internal
    ( ignoreSIGPIPE
    ) where

import Control.Exception (handle, throwIO)
import Foreign.C.Error (Errno(..), ePIPE)
import GHC.IO.Exception (IOErrorType(..), IOException(..))

ignoreSIGPIPE :: IO () -> IO ()
ignoreSIGPIPE = handle (\e -> case e of
    IOError
        { ioe_type = ResourceVanished
        , ioe_errno = Just ioe }
        | Errno ioe == ePIPE -> return ()
    _ -> throwIO e
    )
