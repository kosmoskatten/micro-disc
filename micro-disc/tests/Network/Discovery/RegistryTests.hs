{-# LANGUAGE OverloadedStrings #-}
module Network.Discovery.RegistryTests
    ( registerOneService
    , reRegisterOneService
    , discoverNonExistService
    , discoverExistingService
    , forcefullyRemoveService
    , conditionallyRemoveService
    ) where

import Control.Concurrent.STM (atomically, newTVarIO)
import Control.Monad (void)
import Data.Text (Text)
import Test.HUnit

import Network.Discovery.Info (Info (..))
import Network.Discovery.Registry ( Entry (..)
                                  , empty
                                  , toList
                                  , discoverService
                                  , registerService
                                  , removeService
                                  , removeServiceIf
                                  )

-- | Register one service. Once the registrations is made it shall
-- be visible when listing all the services. The 'Info' listed shall
-- be equal to the one registered and the generation count shall be '1'.
registerOneService :: Assertion
registerOneService = do
    let info = mkInfo "my-serv"
    reg <- newTVarIO empty

    -- Register the service.
    pendings <- atomically $ registerService "my-serv" info reg

    -- Must have no pendings.
    assertEqual "Shall be empty" [] pendings

    -- List all registered entries.
    [(key, entry)] <- atomically $ toList reg

    -- Compare key and entry.
    assertEqual "Key must be equal" "my-serv" key
    assertEqual "Entry must be equal" (Entry 1 info) entry

-- | Re-register one service. Shall have the generation count of '2'.
reRegisterOneService :: Assertion
reRegisterOneService = do
    let info1 = mkInfo "my-serv-1"
        info2 = mkInfo "my-serv-2"
    reg <- newTVarIO empty

    -- Register the service
    void $ atomically $ registerService "my-serv" info1 reg

    -- Re-register the service.
    pendings <- atomically $ registerService "my-serv" info2 reg

    -- Must have no pendings.
    assertEqual "Shall be empty" [] pendings

    -- List all registered entries.
    [(key, entry)] <- atomically $ toList reg

    -- Compare key and entry.
    assertEqual "Key must be equal" "my-serv" key
    assertEqual "Entry must be equal" (Entry 2 info2) entry

-- | Discover a non-existing service. The discover shall give Nothing but
-- a later registrations shall yield a list of reply topics where the
-- discovered topic is found.
discoverNonExistService :: Assertion
discoverNonExistService = do
    let info = mkInfo "my-serv"
    reg <- newTVarIO empty

    -- Try discover the service.
    disc <- atomically $ discoverService "my-serv" "reply-to" reg

    -- Not available now.
    assertEqual "Must be Nothing" Nothing disc

    -- Now, register the service.
    [topic] <- atomically $ registerService "my-serv" info reg

    -- Compare topic.
    assertEqual "Must be requested topic" "reply-to" topic

-- | Discover an existing service. The 'Info' record for the requested
-- service shall be returned immediately.
discoverExistingService :: Assertion
discoverExistingService = do
    let info = mkInfo "my-serv"
    reg <- newTVarIO empty

    -- Register the service.
    void $ atomically $ registerService "my-serv" info reg

    -- Discover the service.
    Just disc <- atomically $ discoverService "my-serv" "reply-to" reg

    -- Compare Info.
    assertEqual "Info shall be equal" info disc

-- | Forcefully remove a registered service. No care is taken about
-- generations.
forcefullyRemoveService :: Assertion
forcefullyRemoveService = do
    let info = mkInfo "my-serv"
    reg <- newTVarIO empty

    -- Register the service.
    void $ atomically $ registerService "my-serv" info reg

    -- And remove it.
    atomically $ removeService "my-serv" reg

    xs <- atomically $ toList reg
    assertEqual "Shall be empty list" [] xs

-- | Conditionally remove a registered service. The condition is expecting
-- a specific generation count for the removal to take place.
conditionallyRemoveService :: Assertion
conditionallyRemoveService = do
    let info = mkInfo "my-serv"
    reg <- newTVarIO empty

    -- Register the service. Its generation count will be 1.
    void $ atomically $ registerService "my-serv" info reg

    -- Try remove it with an "incorrect" generation count. Shall
    -- not succeed.
    succ' <- atomically $ removeServiceIf (== 2) "my-serv" reg
    assertEqual "Shall not have succeeded" False succ'
    xs <- atomically $ toList reg
    assertEqual "Shall be list of length 1" 1 (length xs)

    -- Remove with a correct generation count. Shall succeed.
    succ'' <- atomically $ removeServiceIf (== 1) "my-serv" reg
    assertEqual "Shall have succeeded" True succ''
    xs' <- atomically $ toList reg
    assertEqual "Shall be empty list" [] xs'

mkInfo :: Text -> Info
mkInfo serv =
    Info { service    = serv
         , version    = "0.0.1"
         , interfaces = []
         }
