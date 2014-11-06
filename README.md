IterIO Process Library
----------------------

[![Build Status](https://secure.travis-ci.org/garious/process-iterio.png?branch=master)](http://travis-ci.org/garious/process-iterio)

A small library using the IterIO library to pipe data between processes.

Example usage:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.IterIO
import Data.IterIO.Process

main :: IO ()
main = do
    -- List the contents of the current directory,
    -- grep for a cabal file, and print
    enumProcess "ls" [] |. cmd "grep" ["cabal"] |$ stdoutI

    -- Pipe "very cool" to 'cat' and print the output
    enumPure "very cool\n" |. cmd "cat" [] |$ stdoutI
```


