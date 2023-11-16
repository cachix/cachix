{-# LANGUAGE RankNTypes #-}

module Cachix.Client.Daemon.Log () where

import Control.Concurrent.STM.TBMQueue
import Katip
import Protolude

-- registerScribes = do
--   queue <- newTBMQueueIO 1000
--   mkQueueScribe queue (Katip.permitItem Katip.DebugS) Katip.V2
--
-- -- let logLevel = toKatipLogLevel (daemonLogLevel env)
-- -- let registerScribe = do
-- --       scribeHandle <- Katip.mkHandleScribeWithFormatter Katip.bracketFormat Katip.ColorIfTerminal stdout (Katip.permitItem logLevel) Katip.V2
-- --       Katip.registerScribe "stdout" scribeHandle Katip.defaultScribeSettings (daemonKLogEnv env)
-- -- registerScribe
--
-- mkQueueScribe ::
--   (forall a. (LogItem a) => TBMQueue (Item a)) ->
--   PermitFunc ->
--   Verbosity ->
--   IO Scribe
-- mkQueueScribe queue permitF verb = do
--   return $ Scribe logger closeQueue permitF
--   where
--     logger i =
--       void $ atomically $ tryWriteTBMQueue queue i
--
--     closeQueue = atomically (closeTBMQueue queue)
