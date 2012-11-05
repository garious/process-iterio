IterIO Process Library
----------------------

![alt text](https://travis-ci.org/garious/process-iterio.png "Build Status")

A small library using the IterIO library to pipe data between processes.

Example usage:

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



