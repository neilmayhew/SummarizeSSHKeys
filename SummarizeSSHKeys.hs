import SSHKeys

import Control.Monad
import System.IO
import System.Exit
import Text.Printf

main :: IO ()
main = do
    result <- parseFile "-" <$> getContents
    case result of
        Left e -> do
            hPutStrLn stderr (show e)
            exitFailure
        Right ls -> do
            forM_ ls $ \l -> do
                case l of
                    Entry (o, k, h, c) ->
                        putStrLn $ printf "%s %-7s %-10s %s" (summOpts o) (summKind k) (summHash h) c
                    _ ->
                        return ()

summOpts :: [Option] -> String
summOpts o = printf "[%d]" $ length o

summKind :: String -> String
summKind k =
    case prefix of
        "ssh" -> suffix
        _     -> prefix
  where
    (prefix, rest) = span (/= '-') k
    suffix = drop 1 rest

summHash :: String -> String
summHash = reverse . take 10 . dropWhile (== '=') . reverse
