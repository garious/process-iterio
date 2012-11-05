{-# LANGUAGE OverloadedStrings #-}

import Data.IterIO.Process(cmd, enumProcess)
import Data.IterIO(enumPure, (|.), (|$), stdoutI)

main :: IO ()
main = do
    enumProcess "ls" [] |. cmd "grep" ["cabal"] |$ stdoutI
    enumPure "very cool\n" |. cmd "cat" [] |$ stdoutI


