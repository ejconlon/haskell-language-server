{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Simpletac
  ( descriptor
  , Log (..)
  ) where

import qualified Data.Aeson as A
import Data.Text (Text)
import Development.IDE (IdeState, Rules, RuleResult, define)
import qualified Development.IDE.Core.Shake as Shake
import Development.IDE.Types.Logger (Recorder, WithPriority, Pretty (..), cmapWithPrio, logWith, Priority (..))
import Ide.Types (PluginDescriptor (..), PluginId, defaultPluginDescriptor,
  PluginCommand (..), CommandFunction, mkPluginHandler, PluginHandlers, PluginMethodHandler)
import Language.LSP.Types (Method (..), SMethod (..))
import GHC.Stack (HasCallStack)
import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)

data Log =
    LogShake Shake.Log
  | LogMessage !Text
  deriving stock (Show)

type LogRec = Recorder (WithPriority Log)

logDebug :: (HasCallStack, MonadIO m) => LogRec -> Text -> m ()
logDebug recorder msg = logWith recorder Info (LogMessage msg)

instance Pretty Log where
  pretty = \case
    LogShake l -> pretty l
    LogMessage t -> pretty t

descriptor :: LogRec -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginRules = rules recorder
  , pluginCommands = commands recorder
  , pluginHandlers = handlers recorder
  }

data StubQuery = StubQuery deriving stock (Eq, Ord, Show, Generic)
instance Hashable StubQuery
instance NFData StubQuery

data StubResult = StubResult deriving stock (Eq, Ord, Show, Generic)
instance Hashable StubResult
instance NFData StubResult

type instance RuleResult StubQuery = StubResult

rules :: LogRec -> Rules ()
rules recorder = do
    logDebug recorder "TODO Setting up rules"
    define (cmapWithPrio LogShake recorder) $ \StubQuery _nfp -> do
        logDebug recorder "TODO Handling StubQuery"
        pure ([], Just StubResult)

commands :: LogRec -> [PluginCommand IdeState]
commands recorder =
  [ PluginCommand "doSomething" "Do something" (doSomethingCmd recorder)
  ]

doSomethingCmd :: LogRec -> CommandFunction IdeState ()
doSomethingCmd recorder _state _ = do
    logDebug recorder "TODO Doing something command"
    pure (Right A.Null)

handlers :: LogRec -> PluginHandlers IdeState
handlers recorder = mkPluginHandler STextDocumentHover (hoverProvider recorder)

hoverProvider :: LogRec -> PluginMethodHandler IdeState TextDocumentHover
hoverProvider recorder _state _ _params = do
    logDebug recorder "TODO Providing hover"
    pure (Right Nothing)
