-- Copyright (C) 2014 Swift Navigation Inc.
-- Contact: Fergus Noble <fergus@swift-nav.com>
--
-- This source is subject to the license found in the file 'LICENSE' which must
-- be be distributed together with this source. All other rights reserved.
--
-- THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
-- EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent
import Control.Monad
import qualified System.IO as IO
import Network
import Text.Printf
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.HashMap.Strict as H
import qualified System.Log.Logger as L
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Formatter (simpleLogFormatter)
import Data.Typeable
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT

import Network.Ntrip.Caster.Types
import Network.Ntrip.Caster.NtripServer
import Network.Ntrip.Caster.NtripClient

data CasterException = InvalidRequest
  deriving (Show, Typeable)

instance E.Exception CasterException

listMountpoints :: CasterState -> IO ()
listMountpoints s = do
  mps <- atomically . readTVar $ mountpoints s
  print $ map fst $ H.toList mps

parseRequestLine :: B.ByteString -> Maybe RequestLine
parseRequestLine = parseRequestLine' . filter (not . B.null) . B.split ' '

parseRequestLine' :: [B.ByteString] -> Maybe RequestLine
parseRequestLine' ("SOURCE":pw:mp:[]) = if B.head mp == '/'
                                        then Just $ Source (B.tail mp) pw
                                        else Nothing
parseRequestLine' ("GET":mp:_:[]) = if B.head mp == '/'
                                    then Just $ Get $ B.tail mp
                                    else Nothing
parseRequestLine' _ = Nothing

{-parseHeaders 100 _ = throwIO OverlongHeaders-}
{-parseHeaders count front = do-}
    {-line <- connectionReadLine conn-}
    {-print line-}
    {-if S.null line-}
        {-then return $ front []-}
        {-else do-}
            {-header <- parseHeader line-}
            {-parseHeaders (count + 1) $ front . (header:)-}

{-parseHeader :: B.ByteString -> Maybe Header-}
{-parseHeader h = if B.null val-}
                  {-then Nothing-}
                  {-else Header key-}
                              {-(B.dropWhile (== ' '))-}
                {-where (key, val) = B.breakByte ':' h-}

newConnection :: CasterState -> Connection -> IO ()
newConnection s c = do
  let h = handle c
  -- TODO: Limit line length to prevent malicious client from using up all our
  -- memory.
  req_line <- B.hGetLine h
  let req = parseRequestLine req_line
  case req of
    Just (Get mp)       -> runClient s c mp
    Just (Source mp _) -> runServer s c mp
    Nothing             ->
      L.errorM "caster.connection" $ printf
        "%s:%s : Invalid request '%s'"
        (connHost c) (show $ connPort c) (B.unpack req_line)

-- Create the initial state of the caster.
initialState :: CT.Config -> IO CasterState
initialState cfg = do
  -- The list of mountpoints is stored as a HashMap in an STM TVar, initialise
  -- and empty HashMap and create a new TVar to hold it.
  mps <- newTVarIO H.empty
  return CasterState {
    config = cfg,
    mountpoints = mps
  }

confUpdated :: CT.ChangeHandler
confUpdated k (Just v) =
  L.infoM "caster.config" $ printf "Parameter '%s' changed to %s"
    (T.unpack k) (show v)
confUpdated k Nothing =
  L.infoM "caster.config" $ printf "Parameter '%s' removed (using default)"
    (T.unpack k)

main :: IO ()
main = do
  -- Open the configuration file if present.
  (cfg, _) <- C.autoReload C.autoConfig [C.Optional "caster.conf"]

  -- Register a default handler for all configuration changes so we can log
  -- configuration reloads.
  C.subscribe cfg (C.prefix "caster") confUpdated

  -- Configure hslogger to output everything (DEBUG and above) to stdout.
  log_handler <- streamHandler IO.stdout L.DEBUG >>= \lh -> return $
    setFormatter lh (simpleLogFormatter "[$time $loggername $prio] $msg")
  L.updateGlobalLogger L.rootLoggerName $
    L.setLevel L.DEBUG . L.setHandlers [log_handler]

  -- Create the initial caster state.
  s <- initialState cfg

  -- Start a listener loop forking a new thread and calling newConnection very
  -- time we receive an inbound connection.
  casterPort <- liftM fromIntegral $ C.lookupDefault (2021 :: Int) cfg "caster.port"
  withSocketsDo $ do
    sock <- listenOn $ PortNumber casterPort
    L.infoM "caster" $ printf "Caster listening on port %s" (show casterPort)
    forever $ do
      (h, host, port) <- accept sock
      L.infoM "caster.connection" $
        printf "%s:%s : Connection opened" host (show port)
      let c = Connection { handle = h,
                           connHost = host,
                           connPort = port
                         }
      forkFinally (newConnection s c) $ \result -> do
        -- Ensure the network socket is always cleanly closed.
        IO.hClose h
        -- Handle the connection close (i.e. logging and error reporting).
        handleClose result host port
  where
    -- If the connection thread terminated with an exception, log the error.
    -- If the connection thread exited normally then just note that the
    -- connection was closed.
    handleClose :: Either E.SomeException a -> String -> PortNumber -> IO ()
    handleClose (Left e)  host port = L.errorM "caster.connection" $ printf
      "%s:%s : Connection thread terminated with an exception: %s"
      host (show port) (show e)
    handleClose (Right _) host port = L.infoM "caster.connection" $
        printf "%s:%s : Connection closed" host (show port)

