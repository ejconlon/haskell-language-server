{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Simpletac
  ( descriptor
  , Log (..)
  ) where

import qualified Data.Aeson as A
import Data.Text (Text)
import Development.IDE (IdeState, Rules)
import Development.IDE.Types.Logger (Recorder, WithPriority, Pretty (..), logWith, Priority (..))
import Ide.Types (PluginDescriptor (..), PluginId, defaultPluginDescriptor,
  PluginCommand (..), CommandFunction, mkPluginHandler, PluginHandlers, PluginMethodHandler)
import Language.LSP.Types (Method (..), SMethod (..))
import GHC.Stack (HasCallStack)
import Control.Monad.IO.Class (MonadIO)

data Log =
    LogSomething
  | LogMessage !Text
  deriving stock (Show)

type LogRec = Recorder (WithPriority Log)

logDebug :: (HasCallStack, MonadIO m) => LogRec -> Text -> m ()
logDebug recorder msg = logWith recorder Info (LogMessage msg)

instance Pretty Log where
  pretty = \case
    LogSomething -> "Something"
    LogMessage t -> pretty t

descriptor :: LogRec -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginRules = rules recorder
  , pluginCommands = commands recorder
  , pluginHandlers = handlers recorder
  }

rules :: LogRec -> Rules ()
rules recorder = logDebug recorder "TODO Setting up rules"

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
