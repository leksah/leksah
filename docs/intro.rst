Introduction
============

Leksah is an IDE (Integrated Development Environment) for the
programming language Haskell. It is written in Haskell and integrates
various tools available for writing programs in Haskell: the GHC
compiler and interpreter, the CABAL package management system (the
Common Architecture for Building Applications and Libraries), Haddock
for producing documentation, etc. in one, single, comprehensive and easy
to use environment. It allows the developer to concentrate on writing
the program and Leksah gives him easy access to all information she
needs and helps with the necessary housekeeping for compiling, linking
and package management.

A unified focus for translating source code to executable programs:
Leksah introduces the notion of a workspace that can include several
packages transparently: to the programmer it appears as if there were a
single program with a simple “make” command. Leksah manages rebuilding
and installing packages as far as desired automatically.

Support for writing source code: Leksah supports debugging with GHCi,
evaluation of expressions, gathering type information, setting
breakpoints, displays values at breakpoints, etc. is all possible from
within Leksah.

Last, but not least, Leksah collects information about installed
packages, helps to find function names and their type and offers an
auto-completion feature while you type new code.

The features of Leksah often reflect directly features of the Haskell
tools used; therefore, to understand behavior in special cases needs
sometimes reading the specific documentation of GHC, Cabal or Haddock,
(and this manual, to a degree repeats what is found, with more detail
and authority, in the respective tool documentation).

Leksah is written in Haskell, which means the Leksah developers use
Leksah to develop Leksah and users of Leksah can read the code and
contribute improvements. Leksah uses GTK+ as GUI Toolkit with the gtk2hs
binding. It is platform independent and runs on any platform where GTK+,
gtk2hs and GHC can be installed. It is used on Linux, Windows and Mac.

This document is a reference to the functionality of Leksah, it is not
intended to be a tutorial. Since Leksah is still under development the
information may be incomplete or superseded.

The current version is 0.8.

Further Information
-------------------

The home page for Leksah is `leksah.org <http://leksah.org>`__. Stable
version of Leksah can be installed from Hackage
`hackage.haskell.org/package/leksah <http://hackage.haskell.org/package/leksah>`__\ using
Cabal install. The source code for Leksah is hosted under
`code.haskell.org/leksah <http://code.haskell.org/leksah>`__ and
`code.haskell.org/leksah-head <http://code.haskell.org/leksah-head>`__.
The Leksah user Wiki is
`haskell.org/haskellwiki/Leksah <http://haskell.org/haskellwiki/Leksah>`__.
The Leksah forum can be accessed at
`groups.google.com/group/leksah/topics <http://groups.google.com/group/leksah/topics>`__.
The current version of this manual can be found at
`leksah.org/leksah\_manual.pdf <http://leksah.org/leksah_manual.pdf>`__.
An issue tracker to collect bug reports and suggestions for improvements
is at
`code.google.com/p/leksah/issues/list <http://code.google.com/p/leksah/issues/list>`__.
You can contact the developers at `info (at)
leksah.org <mailto:info@leksah.org>`__.

For information about the Programming language Haskell go to
`www.haskell.org <http://www.haskell.org>`__. The GHC computer is found
at `www.haskell.org/ghc <http://www.haskell.org/ghc>`__. For information
about gtk2hs
`www.haskell.org/gtk2hs/ <http://www.haskell.org/gtk2hs/>`__. For
information about GTK+ go to `www.gtk.org <http://www.gtk.org>`__.

Release Notes
-------------

Version 0.8 Release March 2010
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The 0.8 release adds the notion of workspaces to allow develop
comfortably projects, where part of the code is in separate packages.
This changes the handling of packages to a degree, which has been
improved with introducing suitable defaults: a simple, single-shot
program can be started with very few clicks and entering not much more
than the name of the program; Leksah becomes usable even for just
quickly testing an idea. Other changes include:

-  Better metadata with non exported definitions for workspace packages

-  Support for prebuild metadata packages

-  Better completion (keywords, language extensions, module name, non
   exported definitions)

-  Splittet in a client and server part (Client part doesn’t import
   ghc-api)

-  Added support for Ghc 6.12

-  Prepared for Yi (Abstract TextEditor interface, not ready for use)

-  Various other changes improve usability and stability of the
   platform.

A large number of bugs has been fixed, but there remain, probably a
large number of, bugs - some old and not yet fixed and some new ones. We
expect also to improve and streamline the user interface in the next
minor release to achieve more consistency and make Leksah easier to
learn. You may see comments to this effect in this document, suggesting
possible changes in the interface. Your opinion on these and other
possible improvements you see will be highly appreciated!

Version 0.8 works with GHC 6.10 and 6.12. The installation is described
in for the standard case, more up to date information on installation
may be found on the Wiki
`haskell.org/haskellwiki/Leksah <http://haskell.org/haskellwiki/Leksah>`__.
If you have any trouble installing, please check the Wiki, the forum or
contact the developers to find a solution. A smooth implementation is a
priority for us and we like to hear about difficulties you encounter to
fix them; please report them on the bug and issues tracker
`code.google.com/p/leksah/issues/list <http://code.google.com/p/leksah/issues/list>`__.

Version 0.6 Beta Release Juli 2009
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The 0.6 version introduces an interpreter/debugger mode. This mode can
be switched on and off from the toolbar. In interpreter/debugger mode
expressions can be evaluated and the type of expressions can be
dynamically shown. The GHCi debugger is integrated, so that breakpoints
can be set, it is possible to step through the code, observe the values
of variables and trace the execution history.

The other features of Leksah like building in the background and
reporting errors on the fly work in debugger mode as in compiler mode
(but not configuring, installing, etc. of packages).

Another new feature is integration of grep and text search with regular
expression. This can be accessed from the findbar.

The GUI framework has been enhanced, so that layouts can be nested in so
called group panes. This feature is used for the debugger pane.
Furthermore notebooks can be detached, so that Leksah can be used on
multiple screens.

A lot of little enhancements has been made and numerous bugs has been
fixed.

Known bugs and problems:

-  The package editor works only for cabal files without configurations.

-  MS Windows: The check for external modifications of source files does
   not work.

-  MS Windows: Interruption of a background build does not work.

-  GUI History still not working.

-  Traces pane of the Debugger does not work appropriately.

Version 0.4 Beta Release February/March 2009
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The 0.4 Release is the first beta release of Leksah. It should be usable
for practical work for the ones that wants to engage with it.

It depends on GHC :math:`\geq`\ 6.10.1 and gtk2hs :math:`\geq` 0.10.0.

The class pane and the history feature are not quite ready, so we
propose not to use it yet.

Version 0.1 Alpha Release February 2008
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a pre-release of Leksah. The editor for Cabal Files is not
ready, so we propose not to use it yet. w
