module Envix.Projects
  ( Project (..)
  , Selection
  , find_projects
  , find_projects_by_name
  , implode_home
  , mkproject
  , project_exec
  , project_path
  , sort_projects
  ) where

import qualified Control.Foldl as Fold
import           Control.Monad
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Text (isInfixOf)
import           Envix.Nix
import           Envix.Process
import           Prelude hiding (FilePath)
import           System.Wordexp
import           Turtle hiding (find, sort, sortBy, toText)

type Selection = (Project, Maybe Command)

-- TODO: Add associated action with each project type
-- e.g. for *.nix invoke nix-shell
--      for .git do a fetch?
--      This should be configurable.
-- This can then be paired up with a `--type <type>` cli arg to allow override
-- which action to run. This can obsolete `--no-nix` with `--type plain`.
marker_files :: [(FilePath -> IO Bool, FilePath)]
marker_files = nix_files' ++
                  [(testdir, ".git")
                  ,(testdir, ".hg")
                  ,(testfile, ".project")
                  ]
  where nix_files' = map ((,) testfile) nix_files

find_markers :: FilePath -> IO [FilePath]
find_markers path = do
  is_dir <- isDirectory <$> stat path
  if not is_dir
    then return []
    else fmap snd <$> filterM has_marker marker_files
  where has_marker (check, marker) = check (path </> marker)

mkproject :: FilePath -> Project
mkproject path = Project (filename path) (parent path) []

data Project = Project { project_name :: FilePath
                       , project_dir :: FilePath
                       , project_markers :: [FilePath]
                       }

-- | Replace the value of $HOME in a path with "~"
implode_home :: Project -> IO Project
implode_home project = do
  home' <- home
  let
    path = project_dir project
    dir = case stripPrefix (home' </> "") path of
      Nothing -> path
      Just rest -> "~" </> rest
  return $ project { project_dir = dir }

find_projects_by_name :: FilePath -> [FilePath] -> IO [Project]
find_projects_by_name project = fmap find_matching . find_projects
  where find_matching = filter ((project `isInfix`) . toText . project_name)
        isInfix p = isInfixOf (toText p)
        toText = format fp

find_projects :: [FilePath] -> IO [Project]
find_projects source_dirs = reduce Fold.list $ do
  expanded <- liftIO $ traverse expand_path source_dirs
  candidate <- cat $ map ls (concat expanded)
  markers <- liftIO (find_markers candidate)
  if null markers
    then mzero
    else return Project { project_name = filename candidate
                        , project_dir = parent candidate
                        , project_markers = markers
                        }

expand_path :: FilePath -> IO [FilePath]
expand_path path = do
  expanded <- wordexp nosubst (encodeString path)
  return $ either (const []) (map fromString) expanded

project_path :: Project -> FilePath
project_path (Project name dir _) = dir </> name

sort_projects :: [Project] -> [Project]
sort_projects = sortBy (compare `on` project_name)

project_exec :: (Project -> IO ()) -- ^ Non-nix project action
             -> (FilePath -> IO ()) -- ^ Nix project action
             -> Bool -- ^ Use nix
             -> Project -> IO ()
project_exec plain with_nix use_nix project =
  let action = if use_nix
        then find_nix_file (project_path project)
        else pure Nothing
  in action >>= \case
    Nothing -> plain project
    Just nix_file -> with_nix nix_file
