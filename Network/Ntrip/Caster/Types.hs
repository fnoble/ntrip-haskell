-- Copyright (C) 2014 Swift Navigation Inc.
-- Contact: Fergus Noble <fergus@swift-nav.com>
--
-- This source is subject to the license found in the file 'LICENSE' which must
-- be be distributed together with this source. All other rights reserved.
--
-- THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
-- EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.

module Network.Ntrip.Caster.Types where

import Network
import qualified System.IO as IO
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import Control.Concurrent.STM
import Control.Concurrent.Chan
import qualified Data.Configurator.Types as CT

data Connection = Connection {
  handle :: IO.Handle,
  connHost :: String,
  connPort :: PortNumber
} deriving Show

data ChannelMessage = GnssData B.ByteString
                    | MountpointRemoved

type Channel = Chan ChannelMessage

data Mountpoint = Mountpoint { channel :: Channel }

data Header = Header B.ByteString B.ByteString
  deriving Show

data RequestLine = Get {
                     mountpoint :: B.ByteString
                   }
                 | Source {
                     mountpoint :: B.ByteString,
                     password :: B.ByteString
                   }
  deriving Show

data Request = Request {
                 reqLine :: RequestLine,
                 headers :: [B.ByteString]
               }
  deriving Show

data CasterState = CasterState {
  config :: CT.Config,
  mountpoints :: TVar (H.HashMap B.ByteString Mountpoint)
}

