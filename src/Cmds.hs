module Cmds
  ( OToolOutput
  
  , libNm
  , libUses
  
  , cmdOTool
  , cmdCP
  , cmdChmod
  ) where

-------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------

import           Data.Lens.Common
import           Data.Lens.Template
import           Data.Lens.Strict
import           Data.Typeable
import           Data.List

import           Control.DeepSeq

import           Control.Monad
-- import           Control.Monad.State.Strict
import           Control.Monad.Trans
-- import           Control.Monad.Error.Class

import qualified Control.Exception as CE

-- import           System.Console.GetOpt
-- import           System.Environment
import           System.IO
-- import           System.Exit
import           System.FilePath
-- import           System.Directory
-- import           System.Cmd
import           System.Process
-- import           System.Posix.Process

import           Opts
import           State

-------------------------------------------------------------------------
-- Cmd run
-------------------------------------------------------------------------

showCmdSpec (ShellCommand  s) = s
showCmdSpec (RawCommand f as) = concat $ intersperse " " (f:as)

cmdRunPlain :: Opts -> CreateProcess -> IO ()
cmdRunPlain opts p = do
  when (opts ^. optVerbose) (putStrLn (showCmdSpec $ cmdspec p))
  createProcess p
  return ()

cmdRunOutputPipe :: Opts -> CreateProcess -> IO Handle
cmdRunOutputPipe opts p = do
  when (opts ^. optVerbose) (putStrLn (showCmdSpec $ cmdspec p))
  (_, Just hout, _, _) <- createProcess (p {std_out = CreatePipe})
  return hout

-------------------------------------------------------------------------
-- otool
-------------------------------------------------------------------------

data OToolOutput = OToolOutput
  { _libNm			:: !FilePath
  , _libUses		:: ![FilePath]
  }
  deriving (Show,Typeable)

instance NFData OToolOutput where
  rnf (OToolOutput n u) = rnf n `seq` rnf u

makeLens ''OToolOutput

parseOToolOutput :: String -> OToolOutput
parseOToolOutput s = OToolOutput (takeWhile (/= ':') $ head lib) (map head uses)
  where (lib:uses) = filter (not . null) $ map words $ lines s

otoolOutputFilterOutUnwanted :: Opts -> OToolOutput -> OToolOutput
otoolOutputFilterOutUnwanted opts = libUses ^%= filter (\f -> okExt f && okLoc f)
  where okExt f = takeExtension f `elem` opts ^. optIncludeExtensions
        okLoc f = not $ any (`isPrefixOf` f) $ opts ^. optExcludePrefixes

cmdOTool :: FilePath -> StRun OToolOutput
cmdOTool f = do
  opts <- access opts
  liftIO $ do
    hout <- cmdRunOutputPipe opts $ proc "otool" ["-L", f]
    o <- fmap (otoolOutputFilterOutUnwanted opts . parseOToolOutput) $ hGetContents hout
    o `deepseq` hClose hout
    return o

-------------------------------------------------------------------------
-- cp
-------------------------------------------------------------------------

cmdCP :: String -> FilePath -> FilePath -> StRun ()
cmdCP o fFr fTo = do
  opts <- access opts
  liftIO $ cmdRunPlain opts $ proc "cp" [o, fFr, fTo]

-------------------------------------------------------------------------
-- chmod
-------------------------------------------------------------------------

cmdChmod :: String -> FilePath -> StRun ()
cmdChmod m f = do
  opts <- access opts
  liftIO $ cmdRunPlain opts $ proc "chmod" [m, f]
