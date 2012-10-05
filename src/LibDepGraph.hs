{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module LibDepGraph
  ( LibDepGraph
  , initLibDepGraph
  
  , ldepRoot
  , ldepGraph
  , ldepSymLinks
  
  , ldepResolveSymlink
  )
  where

-------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------

import           Data.Lens.Common
import           Data.Lens.Template
-- import           Data.Graph.GraphVisit
import qualified Data.Map as Map
import           Data.Typeable
-- import qualified Data.Set as Set

-- import           Control.Monad
-- import           Control.Monad.State.Strict
-- import           Control.Monad.Trans
-- import           Control.Monad.Error.Class

-- import qualified Control.Exception as CE

-- import           System.Console.GetOpt
-- import           System.Environment
-- import           System.IO
-- import           System.Exit
-- import           System.FilePath
-- import           System.Directory
-- import           System.Cmd
-- import           System.Process
-- import           System.Posix.Process

-- import           Opts
-- import           State
import           Cmds (OToolOutput)

-------------------------------------------------------------------------
-- Library dependency graph
-------------------------------------------------------------------------

data LibDepGraph = LibDepGraph
  { _ldepRoot		:: FilePath
  , _ldepGraph		:: Map.Map FilePath OToolOutput
  , _ldepSymLinks	:: Map.Map FilePath FilePath
  }
  deriving (Show,Typeable)

initLibDepGraph :: FilePath -> LibDepGraph
initLibDepGraph f = LibDepGraph f Map.empty Map.empty

makeLens ''LibDepGraph

-------------------------------------------------------------------------
-- Resolve symlink via LibDepGraph
-------------------------------------------------------------------------

-- | Resolve symlink via LibDepGraph
ldepResolveSymlink :: LibDepGraph -> FilePath -> FilePath
ldepResolveSymlink ldep fnm = maybe fnm (ldepResolveSymlink ldep) $ Map.lookup fnm $ ldep ^. ldepSymLinks
