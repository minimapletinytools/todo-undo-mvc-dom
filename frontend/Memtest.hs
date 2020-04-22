-- TODO DELETE
{-# LANGUAGE RecursiveDo #-}
module Memtest
  (
  --memtestWidget
  )
where

{-
import           Relude            hiding (All)

import           Reflex
import           Reflex.Dom

import           TodoUndo          hiding (description)
import qualified TodoUndo          as TODO (description)

import           Control.Monad.Fix (MonadFix)
import qualified Data.Map          as Map
import qualified Data.Text         as T
import           Text.Read         (readMaybe)

memtestWidget :: forall t m. (MonadWidget t m) => m ()
memtestWidget = el "div" $ do
  elAttr "section" ("class" =: "todoapp") $ mdo
    dynText (fmap (show . length) tasks)
    addev <- basicButton "add stuff"
    undoev <- basicButton "undo"
    let
      trc = TodoUndoConfig {
          _trconfig_new = fmap (const "hello") addev
          , _trconfig_clearCompleted = never
          , _trconfig_undo            = undoev
          , _trconfig_redo            = never
          , _trconfig_tick            = never
          , _trconfig_remove          = never
        }
      tasks :: Dynamic t [Todo] = _tr_todos todoApp
    todoApp <- holdTodo trc
    liftIO $ print "finish setting up"
  return ()

basicButton
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Text
  -> m (Event t ())
basicButton label = do
  let attrs = "class" =: "basic-button"
  (button, _) <- elAttr' "button" attrs $ text label
  return $ domEvent Click button
-}
