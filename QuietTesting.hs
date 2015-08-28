module QuietTesting where

import Control.Monad (when)
import System.IO (hPutStr, stderr)
import Test.HUnit

runTestTTquiet t = do
    (Counts _ _ e f, ss) <- runTestText putTextToShowS t
    when (e + f /= 0) $
        hPutStr stderr (ss "")
