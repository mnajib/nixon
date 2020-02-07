module Nixon.Select
  ( Select
  , Selection (..)
  , SelectionType (..)
  , Selector
  , Selectable (..)
  , build_map
  , default_selection
  , runSelect
  , select
  , text_to_line
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad.Trans.Reader
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Turtle hiding (f, x, input, select)

data SelectionType = Default | Alternate Int deriving Show
data Selection a = EmptySelection
                 | CanceledSelection
                 | Selection SelectionType a
                 deriving Show

instance Functor Selection where
  fmap f (Selection t x) = Selection t (f x)
  fmap _ EmptySelection = EmptySelection
  fmap _ CanceledSelection =  CanceledSelection

type Selector s = Shell s -> IO (Selection s)

type Select s = ReaderT (Selector s) IO s

-- | Class to represent selectable values
class Selectable s where
  -- | The value to substitute for the selectable entry
  selectValue :: s -> Text

  -- | The pretty-printed value of a selectable (for selection)
  selectDisplay :: s -> Text
  selectDisplay = selectValue

instance Selectable Text where
  selectValue = id

default_selection :: a -> Selection a -> a
default_selection _ (Selection _ value) = value
default_selection def _ = def

build_map :: (a -> Text) -> [a] -> Map.Map Text a
build_map f = Map.fromList . map (f &&& id)

runSelect :: Selector s -> Select s -> IO s
runSelect = flip runReaderT

select :: Selectable s => Shell s -> Select (Selection s)
select input = do
  select' <- ask
  liftIO $ select' input

text_to_line :: Text -> Line
text_to_line = fromMaybe "" . textToLine
