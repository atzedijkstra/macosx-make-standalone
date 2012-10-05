module Opts
  ( ImmediateCommand(..)
  
  , Opts
  
  , optImmediateCommands
  , optVerbose
  , optProgName
  , optExcludePrefixes
  , optIncludeExtensions
  , optInAppLocOfExec			
  , optInAppCpLocOfLibDest		
  , optInAppRenameLocOfLibDest


  , defaultOpts
  , cmdLineOpts
  ) where

-------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------

import           Data.Lens.Common
import           Data.Lens.Template
import           Data.Typeable

import           System.Console.GetOpt

-------------------------------------------------------------------------
-- Immediate commands
-------------------------------------------------------------------------

-- | Immediate command, that is, being done before actually doing the work of the program
data ImmediateCommand
  = ImmediateCommand_Help

-------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------

-- | Options
data Opts 
  = Opts
      { _optVerbose					::	Bool					-- ^ be verbose
      , _optImmediateCommands		::	[ImmediateCommand]		-- ^ e.g. help
      , _optProgName				::	String					-- ^ the name of this program
      , _optExcludePrefixes			::	[FilePath]				-- ^ prefixes of locations which may not be copied
      , _optIncludeExtensions		::	[String]				-- ^ extensions which must be copied (if outside)
      , _optInAppLocOfExec			::  FilePath				-- ^ relative location of executable in app bundle
      , _optInAppCpLocOfLibDest		::  FilePath				-- ^ relative location of where copied libraries should end up in app bundle
      , _optInAppRenameLocOfLibDest	::  FilePath				-- ^ relative location of where to renaming should be done in app bundle
      }
    deriving (Typeable)

-- dylibbundler -od -b -x ./Dazzle.app/Contents/MacOS/Dazzle -d ./Dazzle.app/Contents/libs/ -p @executable_path/../libs/

makeLens ''Opts

defaultOpts 
  = Opts
      { _optVerbose					=	False
      , _optImmediateCommands		=	[]
      , _optProgName				=	"??"
      , _optExcludePrefixes			=   [ "/usr/lib" 
      									, "/System/Library/Frameworks"
      									]
      , _optIncludeExtensions		=   [ ".dylib" 
      									]
      , _optInAppLocOfExec			=	"Contents/MacOS"
      , _optInAppCpLocOfLibDest		=	"Contents/lib"
      , _optInAppRenameLocOfLibDest	=	"@executable_path/../lib"
      }

-------------------------------------------------------------------------
-- Cmdline opts
-------------------------------------------------------------------------

cmdLineOpts
  =  [  Option ""   ["help"]            			(NoArg $ optImmediateCommands ^%= ([ImmediateCommand_Help] ++))
          "output this help"
     ,  Option "v"  ["verbose"]            			(NoArg $ optVerbose ^= True)
          "be verbose"
     ]

