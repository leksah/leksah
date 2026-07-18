Module Browser and Metadata
===========================
**These docs are outdated, we are working on getting them up-to-date for the next release**

o0.3

lc sort & symbol
function &
data &
  constructor &
  slot &
type &
newtype &
class &
  member &
instance &
rule &

Leksah collects data about the modules of all installed Haskell packages
on your system. It does this by reading the Haskell interface files .hi
files (from GHC). It as well collects source positions and comments from
sources. For this it looks in the source directories you specified in
the preferences and downloads and unpacks sources from Hackage depending
on your settings. Starting from the current version, Leksah can as well
use prebuild metadata it might find on the web, to provide metadata for
packages you have sources for, but the call to the Haddock library fails
for some reason.

The packages in the workspace are treated differently, as not only
external exported entities are collected, but all exports from all
modules are collected. As well identifiers, which are not exported from
a module get listed. The source symbol for them is shown in gray.

This metadata is used to answer questions like:

-  Which packages and modules export this identifier?

-  What is the type of the exported identifier?

If the source was found, it lists as well :

-  The comment for this identifier

-  and can mark the item in the source file at the correct position

If you like to get information about some identifier in the code, the
easiest way is to press Ctrl \ and double click on it.

More precisely the operation starts with a release of the left mouse
button with a selection with Ctrl pressed; You can use this if the
double click doesn’t select the intended area. If the identifier is
known unambiguously the modules and info pane will show information
about it. If more than one possibility exist the search pane will open
and present the alternatives.

The sorts of the identifiers shown are differentiated by the symbols you
find in Table [tab:Sorts-of-identifiers]. Note as well the special
symbol for identifiers exposed, but only indirectly, because the
definition is imported from another module.

The Module Browser
------------------

.. figure:: screenshots/screenshot_module_browser.png
   :alt: [fig:Modules-browser]Module browser
   :width: 80.0%

   [fig:Modules-browser]Module browser

The module browser ([fig:Modules-browser]) shows information about
modules and their interface separated in scopes: package, workspace, and
system. If no package or workspace is open only the system scope has
information. (If a workspace/package is open, it’s name(s) are displayed
in the third subdivision from the left of the status and in the title
bar.)

The scope of the displayed information is selected with the radio button
on top of the modules pane: The *Package* scope shows only modules which
are part of the active project. The *Workspace* scope shows all modules
of all packages in the workspace. The *System* scope shows all modules
of installed packages of the system.

(It lists all modules of installed packages. These you would get with
*ghc-pkg* list. Leksah scans the user and the global package database,
when both are present).

The amount of information displayed may overwhelm you with details from
packages that are not of interest to you (Like e.g. like Haskell-98,
ghc, or base-3.0\*). Such packages can be excluded, by blacklisting
them. The packages you want to hide can be specified in the preferences
and you can use the radio button at the right to hide them.

If you select a module in the modules list, its interface is displayed
in the interface list on the right. You can search for a module or
package by selecting the modules list and typing some text. With the up
and down arrows you find the next/previous matching item. With the
escape key or by selecting any other GUI element you leave the search
mode.

If this icon shows up, Leksah has found a source file or source position
for this element. You can open the source file, or bring it to the front
and display the source for the selected location with a *double click*
on the element. (the same is achieved with selecting *Go to definition*
from the context menu.

This is the same as before, but is used for definitions not exported
from the module.

This icon indicates that the symbol is reexported from another module.,
because its long list is not much hierarchically structured.

By selecting an element in the Interface List the so called Info Pane is
shown with detailed information (see next subsection).

The modules pane provides detailed information and are the quickest way
to open a source file for edit. Go to the modules pane, select package
or workspace scope, possibly find the module by entering some text, and
double click on the module’s name to open the file in the editor for
editing the file.

.. figure:: screenshots/screenshot_construct_module.png
   :alt: [fig:Construct-module-dialog]Construct module dialog

   [fig:Construct-module-dialog]Construct module dialog

From the context menu of (right-click) the modules pane you can add a
new module by selecting *Add modul*\ e. The Construct Module dialog will
open ([fig:Construct-module-dialog]). You have to enter the name of the
module, the source path to use if alternatives exist. If the project is
a library you have to specify if the module is exposed. Leksah will
construct the directory, modify the cabal file and construct an empty
module file from a template (The template is stored in the file
module.lksht in the data folder of the project, and will be read from
the .leksah-\*\* folder if you want to provide a different template file
there.

The modification of the cabal file will currently only happen, if it
does not contain configurations.

The Info Pane
~~~~~~~~~~~~~

The Info Pane is the lower pane of the module browser and shows
information about an interface element, which may be a function, a
class, a data definition or a type (selected, for example, in the
modules pane). It shows the identifier, the package and module that it
is exported by, it’s Haskell type and, if found, the Haddock
documentation inserted in the source as a comment.

If you select and initiate an identifier search in an editor pane, the
information about this identifier is automatically displayed in the info
pane (maybe nothing!). The easiest way to do this is to double click on
an identifier while pressing Ctrl.

Only previously collected metadata is available this way. If the item
has changed you could initiate an update of the information collected
with update workspace metadata (menu configuration update workspace
data, or Ctrl-m).

If a source location is attached, you can go to the definition by
clicking the *Source* button.

You can select the module and the interface element in the modules pane
by clicking the *Modules* button.

With the *Refs* button a pane opens which displays modules which uses
this element.

With the *Docu* button you can initiate an external search in a browser
with e.g. Hayoo or Hoogle, depending on the configuration in the
Preferences.

With the *Search* button you can initiate a metadata search for the
identifier.

The Search Pane
---------------

.. figure:: screenshots/screenshot_serach_pane.png
   :alt: [fig:Search-pane]Search pane
   :width: 80.0%

   [fig:Search-pane]Search pane

You can search for an identifier in the metadata by typing in characters
in the entry at the bottom of the pane (not the search entry at the
bottom of the window!). The search result depends on the settings in the
search pane ([fig:Search-pane]). You can choose:

#. The scope in which to search, which can be Package, Workspace or
   System. For Package and Workspace scopes you can search with or
   without imports, which gives 5 different scopes.

#. The way the search is executed, which can be exact, prefix or as a
   regular expression.

#. You can choose if the search shall be case sensitive or not.

The result of the search is displayed in the list part of the Search
pane.

You can see if the module reexports the identifier, or if the source of
the identifier is reachable. When you single click on a search result,
the module browser shows the corresponding information. If you double
click on an entry, the modules and info pane shows the corresponding
information.

If you double click on an identifier while pressing Ctrl in an editor
pane, a case sensitive and exact search in the is started.
