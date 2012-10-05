module Plan
  ( PlanCmd(..)
  , Plan
  
  , planCmdExec
  
  , seqToList
  )
  where

-------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------

import           Data.Lens.Common
import           Data.Lens.Template
-- import           Data.Graph.GraphVisit
-- import qualified Data.Map as Map
import           Data.Sequence
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

import           Opts
import           State
import           Cmds

-------------------------------------------------------------------------
-- Execution plan for modification commands
-------------------------------------------------------------------------

data PlanCmd
  = PlanCmd_CP							-- ^ Copy file
      { _pcFrom		:: FilePath
      , _pcTo		:: FilePath
      }
  | PlanCmd_ModfRef						-- ^ Modify ref in file
      { _pcInFile	:: FilePath
      , _pcFrom		:: FilePath
      , _pcTo		:: FilePath
      }
  | PlanCmd_IntlRename					-- ^ Modify internal name in file
      { _pcInFile	:: FilePath
      , _pcFrom		:: FilePath
      , _pcTo		:: FilePath
      }
  deriving (Show,Typeable)

makeLens ''PlanCmd

type Plan = Seq PlanCmd

-------------------------------------------------------------------------
-- Actual execution
-------------------------------------------------------------------------

planCmdExec :: PlanCmd -> StRun ()
planCmdExec pcmd = case pcmd of
  PlanCmd_CP fFr fTo -> do cmdCP "-f" fFr fTo
                           cmdChmod "+w" fTo
  _ -> return ()

-------------------------------------------------------------------------
-- Seq extension
-------------------------------------------------------------------------

seqToList :: Seq a -> [a]
seqToList s = case viewl s of
  EmptyL -> []
  hd :< tl -> hd : seqToList tl
  
