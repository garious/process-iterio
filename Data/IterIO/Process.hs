{-# LANGUAGE OverloadedStrings #-}

module Data.IterIO.Process
  ( enumProcess
  , cmd
  ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import System.Process hiding (readProcess)
import System.IO(hClose, hFlush, Handle)
import System.Exit(ExitCode(ExitFailure))
import Control.Monad.IO.Class(liftIO, MonadIO)
import Data.IterIO
import Control.Exception(ErrorCall(ErrorCall))

-- Process inum
cmd :: MonadIO m => FilePath -> [String] -> Inum ByteString ByteString m a
cmd p args = inumBracket (mkProc p args) cleanup procInum

-- Process Onum
enumProcess :: FilePath -> [String] -> Onum ByteString IO a
enumProcess p args = inumBracket (mkProc p args) cleanup procInum_

-- Make the process object
mkProc :: MonadIO m => FilePath -> [String] -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
mkProc p args = liftIO $ do
    createProcess (proc p args){
      std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }

-- Cleanup routine for process inums
cleanup :: MonadIO m => (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> m ()
cleanup (Just i, Just o, Just e, _) = liftIO $ do
    hClose i
    hClose o
    hClose e
cleanup _ = error "the impossible happened"

-- Process Inum for an existing process
procInum :: (MonadIO m) => (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> Inum ByteString ByteString m a
procInum (Just i, Just o, Just e, pid) = mkInumM loop
  where
    loop = do
        -- Grab any data we have for stidin
        Chunk t eof <- chunkI
        liftIO $ L.hPut i t

        -- Close stdin if no more data.  This will allow the process exit.
        if eof
          then liftIO $ hClose i
          else liftIO $ hFlush i

        done <- feedStdout i o e pid
        if done
          then return L.empty
          else loop
procInum _ = error "unsupported configuration"


-- Process Onum for an existing process
procInum_ :: (MonadIO m) => (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> Onum ByteString m a
procInum_ (Just i, Just o, Just e, pid) = mkInumM loop
  where
    loop = do
        done <- feedStdout i o e pid
        if done
          then return L.empty
          else loop
procInum_ _ = error "unsupported configuration"


-- TODO: InumState is not exposed by the Inum module.  Is it possible to add a type signature here?
--feedStdout :: (MonadIO m, ChunkData t) => Handle -> Handle -> Handle -> ProcessHandle -> Iter t (IterStateT (InumState t ByteString m a) m) Bool
feedStdout i o e pid = do
    -- Close stdin if no more data.  This will allow the process exit.
    liftIO $ hClose i

    -- After stdin exits, the exe will write to stdout and then close it.
    maybeExitCode <- liftIO $ getProcessExitCode pid

    case maybeExitCode of
      Just (ExitFailure _) -> do
          msg <- liftIO $ B.hGetContents e
          throwI (ErrorCall (B8.unpack msg))

      _ -> return ()

    -- Grab any data we have on stdout
    output <- liftIO $ L.hGetNonBlocking o 1024

    -- Push proc's stdout to the inum's output stream
    notListening <- ifeed output

    -- Debate our future
    return $ notListening || maybeExitCode /= Nothing
