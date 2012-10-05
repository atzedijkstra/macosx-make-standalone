macosx-make-standalone
======================

Modify a MacOSX app bundle to include non standard .dylibs, so it can be run without development environment


Installation
============

- Requires Haskell (7.4.1 onwards)
- Installs via hackage or from this source repo via cabal
  - Hackage:
      cabal install macosx-make-standalone
  - From repo:
      cabal configure
      cabal build
      cabal install


Manual
======

Invocation:
  macosx-make-standalone `mac app bundle`

Options:
- see option --help, currently only verbosity and debug info dump

What is does, restrictions:
- Copies all non /usr/lib `.dylib` files into `mac app bundle/Contents/lib/`
- Changes all references to the old library into refs to the copied libraries, in the `mac app bundle/Contents/MacOS/app name` as well as the copied libraries
- Resolves symbolic links of referenced libraries (thus not for `mac app bundle`)
- In case of duplicate libraries ending up with the same copy name, an arbitrary one is picked (no problem if they are equal)
- Can only be run once, i.e. is not idempotent


Tested with
===========

A single wxHaskell based application, on MacOSX 10.8
