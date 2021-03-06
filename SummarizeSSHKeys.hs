import SSHKeys

import Control.Monad
import Data.Functor
import System.IO
import System.Exit
import Text.Printf

main = do
    result <- parseFile "-" <$> getContents
    case result of
        Left e -> do
            hPutStrLn stderr (show e)
            exitFailure
        Right lines -> do
            forM_ lines $ \l -> do
                case l of
                    Entry (o, k, h, c) ->
                        putStrLn $ printf "%s %-5s %-10s %s" (summOpts o) (summKind k) (summHash h) c
                    _ ->
                        return ()

summOpts o = printf "[%d]" $ length o :: String

summKind k =
    case prefix of
        "ssh" -> suffix
        _     -> prefix
  where
    (prefix, rest) = span (/= '-') k
    suffix = drop 1 rest

summHash = reverse . take 10 . dropWhile (== '=') . reverse
