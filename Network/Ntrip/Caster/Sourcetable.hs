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

module Network.Ntrip.Caster.Sourcetable (
  runSourcetable
) where

import Text.Printf
import qualified System.IO as IO
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as H
import qualified Data.Configurator as C

import Network.Ntrip.Caster.Types

runSourcetable :: CasterState -> Connection -> IO ()
runSourcetable s c = do
  let h = handle c
  B.hPutStrLn h "SOURCETABLE 200 OK"
  mps <- atomically . readTVar $ mountpoints s
  let st_bs = B.pack $ show $ map fst $ H.toList mps
      len = B.length st_bs

  caster_id <- C.lookupDefault ("SwiftNtripCaster" :: String) (config s) "caster.identifier"
  IO.hPutStrLn h $ printf "Server: %s/1.0" caster_id
  IO.hPutStrLn h $ "Content-Type: text/plain"
  IO.hPutStrLn h $ printf "Content-Length: %d\n" len
  B.hPutStrLn h st_bs

