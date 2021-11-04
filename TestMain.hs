module Main where

import Control.Monad (when)
import System.Exit
import Test.HUnit

import SSHKeys as S

alltests :: Test
alltests = test
    [ "SSHKeys" ~: S.tests ]

main :: IO ()
main = do counts <- runTestTT alltests
          when ((errors counts, failures counts) /= (0, 0)) $
              exitWith (ExitFailure 1)
