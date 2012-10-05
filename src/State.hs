module State
  ( ImmediateCommand(..)
  
  , RunEnv(RunEnv)

  , State
  , initSt

  , opts
  , pid
  
  , StRun
  
  , srRmFilesToRm
  , srFreshTmpName
  , srRegisterFileForRm
  ) where

-------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------

import           Data.Lens.Common
import           Data.Lens.Template
import           Data.Lens.Strict
import           Data.Typeable
-- import qualified Data.Set as Set

import           Control.Monad.State.Strict
import qualified Control.Exception as CE

import           System.Posix.Types
import           System.FilePath
import           System.IO
-- import           System.IO.Temp
import           System.Directory

import           Opts

-------------------------------------------------------------------------
-- Bits of info about runtime env
-------------------------------------------------------------------------

data RunEnv = RunEnv
  { _tmpdir		:: FilePath
  , _pid		:: ProcessID
  , _argFile	:: FilePath
  }
  deriving Typeable

makeLens ''RunEnv

-------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------

data St = St
  { _opts		:: Opts
  , _runEnv		:: RunEnv
  , _uniq		:: Int
  , _filesToRm	:: [FilePath]
  }
  deriving Typeable

initSt :: Opts -> RunEnv -> St
initSt o e = St o e 0 []

makeLens ''St

-------------------------------------------------------------------------
-- St monad
-------------------------------------------------------------------------

type StRun a = StateT St IO a

-- | get a fresh uniq nr
srFreshUniq :: StRun Int
srFreshUniq = do
  i <- access uniq
  uniq += 1
  return i

-- | register file for removal
srRegisterFileForRm :: FilePath -> StRun [FilePath]
srRegisterFileForRm f = filesToRm %= ([f]++)

-- | remove all files registered for removal
srRmFilesToRm :: StRun ()
srRmFilesToRm = do
  files <- filesToRm %%= (\f -> (f,[]))
  liftIO $ forM_ files rm
 where rm f = CE.catch (removeFile f)
                       (\(e :: CE.SomeException) -> hPutStrLn stderr (show f ++ ": " ++ show e))

-- | get a fresh tmp filename
srFreshTmpName :: StRun FilePath
srFreshTmpName = do
  i <- srFreshUniq
  e <- access runEnv
  let t = e ^. tmpdir </> takeBaseName (e ^. argFile)  ++ "-" ++ show (e ^. pid) ++ "-" ++ show i
  srRegisterFileForRm t
  return t

