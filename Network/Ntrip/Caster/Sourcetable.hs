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

import qualified System.IO as IO
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as H

import Network.Ntrip.Caster.Types

runSourcetable :: CasterState -> Connection -> IO ()
runSourcetable s c = do
  let h = handle c
  B.hPutStrLn h "SOURCETABLE 200 OK"
  mps <- atomically . readTVar $ mountpoints s
  IO.hPrint h $ map fst $ H.toList mps

