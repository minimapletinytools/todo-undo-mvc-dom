-- a lot of code copied from https://github.com/reflex-frp/reflex-todomvc

{-# LANGUAGE RecursiveDo #-}
module Todo (
  todoWidget
) where

import           Relude            hiding (Op)

import           Reflex
import           Reflex.Dom

import           Control.Monad.Fix (MonadFix)
import qualified Data.Text         as T
import           Text.Read         (readMaybe)

-- | Strip leading and trailing whitespace from the user's entry, and discard it if nothing remains
stripDescription :: Text -> Maybe Text
stripDescription d =
  let trimmed = T.strip d
  in if T.null trimmed
     then Nothing
     else Just trimmed


taskEntry
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => m (Event t Text)
taskEntry = el "header" $ do
  -- Create the textbox; it will be cleared whenever the user presses enter
  rec let newValueEntered = keypress Enter descriptionBox
      descriptionBox <- inputElement $ def
        & inputElementConfig_setValue .~ fmap (const "") newValueEntered
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            mconcat [ "class" =: "new-todo"
                    , "placeholder" =: "What needs to be done?"
                    , "name" =: "newTodo"
                    , "type" =: "text"
                    ]
  -- -- Request focus on this element when the widget is done being built
  -- schedulePostBuild $ liftIO $ focus $ _textInput_element descriptionBox
  let -- | Get the current value of the textbox whenever the user hits enter
      newValue = tag (current $ value descriptionBox) newValueEntered
  -- -- Set focus when the user enters a new Task
  -- performEvent_ $ fmap (const $ liftIO $ focus $ _textInput_element descriptionBox) newValueEntered
  return $ fmapMaybe stripDescription newValue


todoWidget :: forall t m. (MonadWidget t m) => m ()
todoWidget = do
  el "header" $ do
    el "h1" $ text "todos"
    newTodoEv <- taskEntry
    return ()
  return ()
