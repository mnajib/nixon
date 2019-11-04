module Envix.Projects.Types
  ( Cmd (..)
  , ProjectType (..)
  , find_markers
  , proj
  , project_types
  , resolve_commands
  , test_marker
  ) where

import           Control.Monad (filterM)
import qualified Data.Text as T
import           Envix.Nix
import           Envix.Process
import           Prelude hiding (FilePath)
import           Turtle

-- TODO: Parse e.g. package.json for npm scripts?
-- TODO: Add associated action with each project type
-- e.g. for *.nix invoke nix-shell
--      for .git do a fetch?
--      This should be configurable.
-- This can then be paired up with a `--type <type>` cli arg to allow override
-- which action to run. This can obsolete `--no-nix` with `--type plain`.
-- TODO: Add support for local overrides with an .envix project file
-- TODO: Record commands made within a project and add to list
-- TODO: List descriptions
-- TODO: Add command type: data CmdType = Terminal | GUI
project_types :: [ProjectType]
project_types =
  [proj ["cabal.project"] "Cabal new-style project"
   [Cmd "cabal" ["new-build"] "build"
   ,Cmd "cabal" ["new-repl"] "repl"
   ,Cmd "cabal" ["new-run"] "run"
   ,Cmd "cabal" ["new-test"] "test"
   ]
  ,proj ["package.json"] "NPM project"
   [Cmd "npm" ["install"] "install"
   ,Cmd "npm" ["start"] "run"
   ,Cmd "npm" ["test"] "test"
   ]
  ,proj (map ProjectPath nix_files) "Nix project"
   [Cmd "nix-build" [] "build"
   ,Cmd "nix-shell" [] "shell"
   ]
  ,proj [".envrc"] "Direnv project"
   [Cmd "direnv" ["allow"] "direnv allow"
   ,Cmd "direnv" ["deny"] "direnv deny"
   ,Cmd "direnv" ["reload"] "direnv reload"
   ]
  ,proj [".git"] "Git repository"
   [Cmd "git" ["fetch"] "Git fetch"
   ,Cmd "git" ["log"] "Git log"
   ,Cmd "git" ["rebase"] "Git rebase"
   ,Cmd "git" ["status"] "Git status"
   ]
  ,proj [".hg"] "Mercurial project" []
  ,proj [".project"] "Ad-hoc project" []
  ,proj [ProjectFunc . const $ pure True] "Generic project"
   [Cmd "x-terminal-emulator" [] "Terminal"
   ,Cmd "emacs" [] "Emacs"
   ,Cmd "vim" [] "Vim"
   ,Cmd "dolphin" [ArgPath] "Files"
   ,Cmd "rofi" ["-show", "run"] "Run"
   ]
  ]

data ProjectType = ProjectType { project_markers :: [ProjectMarker]
                               , project_description :: Text
                               , project_commands :: [Cmd]
                               }

proj :: [ProjectMarker] -> Text -> [Cmd] -> ProjectType
proj = ProjectType

data ProjectMarker = ProjectPath FilePath
                   | ProjectFile FilePath
                   | ProjectDir FilePath
                   | ProjectFunc (FilePath -> IO Bool)

instance IsString ProjectMarker where
  fromString = ProjectPath . fromText . T.pack

instance Show ProjectMarker where
  show (ProjectFunc _) = "ProjectFunc (..)"
  show (ProjectPath path) = "ProjectPath" ++ show path
  show (ProjectFile path) = "ProjectFile" ++ show path
  show (ProjectDir path)  = "ProjectDir"  ++ show path

-- | Test that a marker is valid for a path
test_marker :: ProjectMarker -> FilePath -> IO Bool
test_marker (ProjectPath marker) path = testpath (path </> marker)
test_marker (ProjectFile marker) path = testfile (path </> marker)
test_marker (ProjectDir  marker) path = testdir (path </> marker)
test_marker (ProjectFunc marker) path = marker path

-- | Given a path, find markers and associated commands.
find_markers :: FilePath -> IO [Cmd]
find_markers path = do
  is_dir <- isDirectory <$> stat path
  if not is_dir
    then return []
    else concatMap project_commands <$> filterM has_markers project_types
  where has_markers = fmap and . traverse (`test_marker` path) . project_markers

data Argument = ArgText Text | ArgPath
              deriving Show

instance IsString Argument where
  fromString = ArgText . T.pack

data Cmd = Cmd { _cmd :: Text
               , _args :: [Argument]
               , _desc :: Text
               } deriving Show

resolve_commands :: FilePath -> [Cmd] -> [Command]
resolve_commands path commands = to_command <$> commands
  where
        to_command (Cmd c a _) = Command c (map expand_arg a)
        expand_arg (ArgText t) = t
        expand_arg ArgPath = format fp path