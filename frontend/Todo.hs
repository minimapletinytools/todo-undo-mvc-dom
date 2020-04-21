-- a lot of code copied from https://github.com/reflex-frp/reflex-todomvc

{-# LANGUAGE RecursiveDo #-}
module Todo (
  todoWidget
) where

import           Relude            hiding (All)

import           Reflex
import           Reflex.Dom

import           TodoUndo          hiding (description)
import qualified TodoUndo          as TODO (description)

import           Control.Monad.Fix (MonadFix)
import qualified Data.Map          as Map
import qualified Data.Text         as T
import           Text.Read         (readMaybe)

todoWidget :: forall t m. (MonadWidget t m) => m ()
todoWidget = el "div" $ do
  elAttr "section" ("class" =: "todoapp") $ do
    mainHeader
    rec
      newTask <- taskEntry
      (activeFilter, clearCompleted) <- controls tasks
      undoEv <- basicButton "undo"
      redoEv <- basicButton "redo"
      dynText (fmap show activeFilter)
      let
        trc = TodoUndoConfig {
            _trconfig_new = newTask
            , _trconfig_clearCompleted = clearCompleted
            , _trconfig_undo            = undoEv
            , _trconfig_redo            = redoEv
            , _trconfig_tick            = toggleEv
            , _trconfig_untick          = never
            , _trconfig_remove          = destroyEv
          }
        tasks :: Dynamic t [Todo] = _tr_todos todoApp
      todoApp <- holdTodo trc
      (toggleEv, destroyEv, modifyEv) <- taskList (traceDyn "filter" activeFilter) tasks
      liftIO $ print "finish setting up"
    return ()
  infoFooter



-- | Extract the 'fst' of a triple.
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

-- | Extract the 'snd' of a triple.
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

-- | Extract the final element of a triple.
thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

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


basicButton
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Text
  -> m (Event t ())
basicButton label = do
  let attrs = "class" =: "toggle-all"
  (button, _) <- elAttr' "button" attrs $ text label
  return $ domEvent Click button

-- | Display the main header
mainHeader :: DomBuilder t m => m ()
mainHeader = el "h1" $ text "todos"

-- | Display static information about the application
infoFooter :: DomBuilder t m => m ()
infoFooter = elAttr "footer" ("class" =: "info") $ do
  el "p" $ text "Click to edit a todo"
  el "p" $ do
    text "Written by "
    elAttr "a" ("href" =: "https://github.com/pdlla") $ text "pdlla"

buildActiveFilter
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => m (Dynamic t Filter)
buildActiveFilter = elAttr "ul" ("class" =: "filters") $ do
  rec activeFilter <- holdDyn All setFilter
      let filterButton f = el "li" $ do
            let buttonAttrs = ffor activeFilter $ \af -> "class" =: if f == af then "selected" else ""
            (e, _) <- elDynAttr' "a" buttonAttrs $ text $ T.pack $ show f
            return $ fmap (const f) (domEvent Click e)
      allButton <- filterButton All
      text " "
      activeButton <- filterButton Active
      text " "
      completedButton <- filterButton Completed
      let setFilter = leftmost [allButton, activeButton, completedButton]
  return activeFilter

-- | Display the control footer; return the user's currently-selected filter and an event that fires when the user chooses to clear all completed events
controls
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t [Todo]
  -> m (Dynamic t Filter, Event t ())
controls tasks = do
  -- Determine the attributes for the footer; it is invisible when there are no todo items
  let controlsAttrs = ffor tasks $ \t -> "class" =: "footer" <> if null t then "style" =: "visibility:hidden" else mempty
  elDynAttr "footer" controlsAttrs $ do
    -- Compute the number of completed and uncompleted tasks
    let (tasksCompleted, tasksLeft) = splitDynPure $ ffor tasks $ \m ->
          let completed = length . filter isDone $ m
          in (completed, length m - completed)
    elAttr "span" ("class" =: "todo-count") $ do
      el "strong" $ dynText $ fmap (T.pack . show) tasksLeft
      dynText $ fmap (\n -> (if n == 1 then " item" else " items") <> " left") tasksLeft
    activeFilter <- buildActiveFilter
    let clearCompletedAttrs = ffor tasksCompleted $ \n -> mconcat
          [ "class" =: "clear-completed"
          , if n > 0 then mempty else "hidden" =: ""
          ]
    (clearCompletedAttrsButton, _) <- elDynAttr' "button" clearCompletedAttrs $ dynText $ ffor tasksCompleted $ \n -> "Clear completed (" <> T.pack (show n) <> ")"
    return (activeFilter, domEvent Click clearCompletedAttrsButton)





-- | Display the user's Tasks, subject to a Filter; return requested modifications to the Task list
taskList
  :: forall t m. ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadIO m
     )
  => Dynamic t Filter
  -> Dynamic t [Todo]
  -- TODO add change event
  -> m (Event t Int, Event t Int, Event t (Int, Text))  -- (toggle, remove, modify)
taskList activeFilter tasks = elAttr "section" ("class" =: "main") $ do
  let toggleAllState = all isDone <$> tasks
      toggleAllAttrs = ffor tasks $ \t -> "class" =: "toggle-all" <> "name" =: "toggle" <> if null t then "style" =: "visibility:hidden" else mempty
  -- TODO why does this break???
  --toggleAll <- toggleInput toggleAllAttrs toggleAllState
  elAttr "label" ("for" =: "toggle-all") $ text "Mark all as complete"
  -- Filter the item list

  --let
  let
    -- TODO need to zip with index too
    visibleTasks :: Dynamic t [(Int, Todo)]
    --visibleTasks = zipDynWith (Map.filter . satisfiesFilter) activeFilter tasks
    visibleTasks = fmap (zip [0..]) tasks

  -- Hide the item list itself if there are no items
    itemListAttrs = ffor visibleTasks $ \t -> mconcat
      [ "class" =: "todo-list"
      , if null t then "style" =: "visibility:hidden" else mempty
      ]
  liftIO $ print "5"
  -- Display the items
  itemEvs :: Dynamic t [(Event t Int, Event t Int, Event t (Int, Text))]
    <- elDynAttr "ul" itemListAttrs $ simpleList visibleTasks todoItem
  let
    splitItemEvs :: Dynamic t ([Event t Int], [Event t Int], [Event t (Int, Text)]) = fmap unzip3 itemEvs
    toggleEvs = switchDyn . fmap leftmost . fmap fst3 $ splitItemEvs
    destroyEvs = switchDyn . fmap leftmost . fmap snd3 $ splitItemEvs
    modifyEvs = switchDyn . fmap leftmost . fmap thd3 $ splitItemEvs

  liftIO $ print "6"

  -- we assume only one item changes at once ever D:, just do leftmost I guess
  -- you could do the repeatEvent thing if you really wanted to
  return (toggleEvs, destroyEvs, modifyEvs)

toggleInput
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t (Map AttributeName Text)
  -> Dynamic t Bool
  -> m (Event t ())
toggleInput dynAttrs dynChecked = do
  let attrs = (<> "class" =: "toggle") . ("type" =: "checkbox" <>) <$> dynAttrs
      updatedAttrs = fmap Just <$> updated dynAttrs
      updatedChecked = updated dynChecked
  initialAttrs <- sample $ current attrs
  initialChecked <- sample $ current dynChecked
  domEvent Click <$> inputElement (def
    & inputElementConfig_initialChecked .~ initialChecked
    & inputElementConfig_setChecked .~ updatedChecked
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ updatedAttrs
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initialAttrs)

buildCompletedCheckbox
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t Todo
  -> Dynamic t Text
  -> m (Event t (), Event t (), Event t ()) -- ^ (toggle, clear, modify)
buildCompletedCheckbox todo description = elAttr "div" ("class" =: "view") $ do
  -- Display the todo item's completed status, and allow it to be set
  completed <- holdUniqDyn $ fmap isDone todo
  checkboxClicked <- toggleInput (constDyn mempty) completed
  let setCompleted = fmap (const ()) $ checkboxClicked
  -- Display the todo item's name for viewing purposes
  (descriptionLabel, _) <- el' "label" $ dynText description
  -- Display the button for deleting the todo item
  (destroyButton, _) <- elAttr' "button" ("class" =: "destroy") $ return ()
  return ( setCompleted
         , domEvent Click destroyButton
         , void $ domEvent Dblclick descriptionLabel
         )

-- | Display an individual todo item
todoItem
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t (Int, Todo)
  -> m (Event t Int, Event t Int, Event t (Int, Text)) -- ^ (toggle, destroy, modify)
todoItem todoWithId = mdo
  let
    todo = fmap snd todoWithId
    todoId = current $ fmap fst todoWithId
  description <- holdUniqDyn $ fmap TODO.description todo
  -- Construct the attributes for our element
  let attrs = ffor2 todo editing' $ \t e -> Map.singleton "class" $ T.unwords $ concat
        [ [ "completed" | isDone t ]
        , [ "editing" | e ]
        ]
  (editing', toggleEv', destroyEv', modifyEv') <- elDynAttr "li" attrs $ do
    (toggleEv, destroyEv, startEditing) <- buildCompletedCheckbox todo description
    -- Set the current value of the editBox whenever we start editing (it's not visible in non-editing mode)
    let setEditValue = tag (current description) $ ffilter id $ updated editing'
    editBox <- inputElement $ def
      & inputElementConfig_setValue .~ setEditValue
      & inputElementConfig_elementConfig . elementConfig_initialAttributes
        .~ ("class" =: "edit" <> "name" =: "title")
    let -- Set the todo item's description when the user leaves the textbox or presses enter in it
        setDescription = tag (current $ value editBox) $ leftmost
          [ keypress Enter editBox
          , domEvent Blur editBox
          ]
        -- Cancel editing (without changing the item's description) when the user presses escape in the textbox
        cancelEdit = keypress Escape editBox
        modifyEv = attachWith (\i d -> (i,d)) todoId setDescription
    -- Set focus on the edit box when we enter edit mode
--        postGui <- askPostGui
--        performEvent_ $ fmap (const $ liftIO $ void $ forkIO $ threadDelay 1000 >> postGui (liftIO $ focus $ _textInput_element editBox)) startEditing -- Without the delay, the focus doesn't take effect because the element hasn't become unhidden yet; we need to use postGui to ensure that this is threadsafe when built with GTK
    -- Determine the current editing state; initially false, but can be modified by various events
    editing <- holdDyn False $ leftmost [ fmap (const True) startEditing
                                        , fmap (const False) setDescription
                                        , fmap (const False) cancelEdit
                                        ]
    return (editing, tag todoId toggleEv, tag todoId destroyEv, modifyEv)
  -- Return an event that fires whenever we change ourselves
  return (toggleEv', destroyEv', modifyEv')





--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

-- | Subsets of the task list that can be selected by the user
data Filter = All
    | Active
    | Completed
    deriving (Show, Eq)

-- | Determine whether this Task should be shown when this Filter is in effect
satisfiesFilter :: Filter -> Todo -> Bool
satisfiesFilter f = case f of
  All       -> const True
  Active    -> not . isDone
  Completed -> isDone
