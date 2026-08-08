Installing Leksah[sec:Installing-Leksah]
========================================
**These docs are outdated, we are working on getting them up-to-date for the next release**

How to Install: Brief Instructions
-----------------------------------

You can install from

-  a binary installer for your operating system, which is typically
   Windows or Macintosh.

-  a package for your platform, which is currently Arch Linux and Fedora
   Linux, and Debian(Ubuntu) is in preparation.

-  install from sources from Hackage via ``cabal install leksah``

-  leksah or leksah-head development repositories. (If you want the very
   last or want to help with Leksah development).

You can consult the Download page for up-to date information and try the
user Wiki for further help.

Microsoft Windows
-----------------

#. Install Haskell Platform with an installer for Windows (alternatively
   install Ghc directly)

#. Make sure wget and grep are on the path of your Windows shell

#. Install Leksah from the most recent binary installer for Windows.

#. Go to the post installation section.

Mac OS X
--------

#. Install Haskell Platform with an installer for Mac OS X
   (alternatively install Ghc directly)

#. Make sure wget and grep are on the path.

#. Install Leksah from the most recent binary installer for Mac

#. Go to the post installation section.

Linux from Distro Packages
--------------------------

#. Install Leksah with the package management system of your Linux
   platform, which should pull all prerequisites automatically.

#. Go to the post installation section.

Install from Hackage
--------------------

#. Install Haskell Platform (alternatively install Ghc directly, install
   Cabal and cabal-install)

#. Install gtk2hs in a version compatible with the installed Ghc
   compiler (Currently gtk2hs can’t be installed via Hackage, but this
   should be possible in the near future, so that you don’t have to care
   about this step any more). Make sure the gtk2hs gtksourceview2
   package gets built and installed.

#. open a Console and do:

   | cabal update 
   | cabal install leksah

#. Go to the post installation section.

Post Installation steps
-----------------------

#. Until the next release of gtk2hs, for a pleasant visual appearance,
   you have to copy or append the .gtkrc-2.0 file from the Leksah data
   folder or from the data folder in Leksah sources to your home folder.
   If you miss the step, the cross [x] buttons on tabs are almost
   invisible (or don’t fit in tabs). This step may become obsolete
   during the 0.8 release cycle.

   | cd ~  
   | wget http://code.haskell.org/leksah/leksah/data/.gtkrc-2.0 -O  
   |                       .gtkrc-2.0-leksah 
   | echo -e ’\\ninclude .gtkrc-2.0-leksah’ >> .gtkrc-2.0 

#. Before you start Leksah for the first time, do a:

   ghc-pkg recache

   It has been observed, that a package recache is often necessary after
   installation. The symptom is an empty Module Browser, if you select
   the System scope.

First start of Leksah
---------------------

The first time you start Leksah it will take you through the follow
steps:

#. You are asked to fill in a form telling Leksah where your Haskell
   sources are (if you are not sure or just want to test, you can accept
   the defaults and correct them later in the “metadata” preferences)

#. Leksah collects “metadata”, i.e. exported symbols and their type,
   comments explaining them etc. for all installed packages on your
   machine. This step may take a while and may give no feedback or a lot
   of strange errors and warnings, don’t worry but be patient.

#. The Leksah IDE starts and you can start working.

Later starts will read in the previously collected metadata and check
only for changes. After starting up, Leksah will open its Main window in
a standard configuration.

Progress on your first contact with Leksah:

#. Start with the, infamous, “Hello World” example. The next section
   gives you a step by step description.

#. Then it might be the best to construct a workspace and add an
   existing project and explore Leksah while you work on it.

First start dialog
------------------

.. figure:: screenshots/screenshot_first_start.png
   :alt: [fig:FirstStart-dialog]First-Start dialog
   :width: 90.0%

   [fig:FirstStart-dialog]First-Start dialog

When you start Leksah for the first time it must collect the information
about the packages you have on your computer and may use in your
projects. The first start dialog let you enter settings about this
process. Leksah then collects information about exported symbols, their
type and possible comments (collectively called metadata) to support
your work, e.g. by suggesting auto-completion and type information about
functions you may use while you edit your source.

Later you can change this settings in the preferences pane in Leksah and
you can rebuild the metadata at any time.
``leksah-server -sbo +RTS -N2`` from the console. Details about metadata
collection can be found here: [sub:Metadata-collection].

If you want to start from scratch again delete or rename the
.leksah-\*.\* folder in your home folder. Then you will see the first
start dialog again.

In the first start dialog you are asked for:

#. The location of folders, where Haskell source code for installed
   packages can be found. This is important for packages which can’t be
   found on Hackage.

#. Maybe a directory, where Leksah will unpack source files for
   packages. If you give no directory here, Leksah will not try to
   unpack the sources.

#. Some packages are difficult to process with Haddock. So we provide
   some prebuild metadata. If you allow this, Leksah will look for
   prebuild metadata, if sources are available, but Haddock fails to
   process.

#. The port number used for the local connection to the Leksah server.

#. By default the Leksah server terminates with the last connection. You
   can change this setting here.

Leksah collects information about all installed packages on your system
that will take some time (minutes to half an hour) the first time.
Errors occurring in this metadata collection step indicate only that
Leksah has not succeeded to extract the source locations and comments
from a module or package; they are not consequential, except that some
metainformation may be missing. The metadata is cached and future starts
only scan newly installed packages, starts only information for new
packages will be installed.

.. figure:: screenshots/screenshot_welcome.png
   :alt: Leksah after first start
   :width: 100.0%

   Leksah after first start
