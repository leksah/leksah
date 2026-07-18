Hello World example
===================
**These docs are outdated, we are working on getting them up-to-date for the next release**

-  Workspace -> New. Constructs a new workspace in a selected folder and
   give it a name (e.g. “Hello”). This produces a file Hello.lkshw.

-  Package -> New and use the Create Folder button to make a new folder
   for the package. Make sure to be in this folder when you click
   “Open”. An editor opens up, which let you edit cabal files. The name
   proposed for your package is the name of the folder you just
   constructed. That is the convention with Cabal packages. The defaults
   are set for creating a simple executable. The base package is
   specified as build dependencies, and an executable with the name of
   the package will be constructed. The main module resides in a file
   “Main.hs”. The sources are in a “src” subdirectory of the packages.

-  Click Save to write the .cabal file.

-  The main module gets automatically constructed and opens.

-  Now add your code to the module

   *main = putStrLn Hello World . *

-  By default, auto build is on and you can see that the file will be
   compiled in the Log pane.

-  Choose Package -> Run or ctrl-alt-r, and you will see Hello World in
   the Log pane.

-  Or: Choose Package -> Install, open a Shell and try out your newly
   created executable

Congratulations ! you have now entered, compiled, linked and run your
first Haskell program with Leksah. It is as easy as: Create workspace /
New package / enter your code/ Run. Remember:

-  the project folder is the folder in which your .cabal file for the
   project is stored.

-  A workspace is just a file, which contain information about included
   packages.

-  You see what Leksah is doing by observing the output from the Log
   window.

Furthermore:

-  You can add Packages with the context menu in the workspace pane. You
   can construct new packages with Package -> New, from the menubar.

-  You can add other modules by selecting “Add module” from the context
   menu of the modules pane of the browser group.

-  You can open panes you need by selecting Panes -> Browser \| Log \|
   ... from the menubar.

-  You can editing modules by selecting them in the Browser. You can
   search in the modules pane of the browser by typing text.

You may as easily debug it

-  Switch debugger Mode on.

-  Pane -> Debugger

-  Select the word main in your code

-  Right click and choose Eval from the pop-up menu or press ctrl-enter.

-  Switch of debugger Mode if you want to compile an executable.
