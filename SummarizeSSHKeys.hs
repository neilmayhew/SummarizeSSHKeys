{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import SSHKeys

import Control.Monad
import Data.Foldable
import Options.Applicative
import System.Exit
import System.IO
import Text.Printf

import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optFiles :: [FilePath]
  } deriving (Show)

main :: IO ()
main = do

  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <- customExecParser
    ( prefs $ columns cols )
    ( info
      ( helper <*> do
          let strArguments mods = (:) <$> strArgument mods <*> many (strArgument mempty)
          optFiles <- strArguments $
            metavar "KEYSFILE ..." <> value "/dev/stdin" <>
            help "Files containing SSH public keys" <> showDefaultWith id
          pure Options{..}
      )
      ( fullDesc <> header "Output readable summaries of SSH authorized_keys files" )
    )

  for_ optFiles $ \f -> do
    result <- parseFile f <$> readFile f
    case result of
        Left e -> do
            hPrint stderr e
            exitFailure
        Right ls -> do
            forM_ ls $ \case
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
