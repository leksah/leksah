Contributing to Leksah
===
*This document is still work in progress*

This document is meant to get you started hacking on the Leksah code base.

* Join us at `#leksah` on on freenode if you have any questions

##Getting started

1. Click "Fork" at the top of the leksah repository page
2. Clone your fork locally
    ```
    git clone <url-to-your-fork>
    ```

3. Make sure you can build leksah from source (See [README.md](https://github.com/leksah/leksah/blob/master/Readme.md))

Making a contribution
===

1. Have a look at the [issue tracker](https://github.com/leksah/leksah/issues) or think of something new
2. Create a new branch to work in
    ```
    git checkout -b my-branch-name
    ```

3. Haskell programming
4. Commit your changes
    ```
    git commit -am "Some useful message"
    ```

5. Push the branch to your forked repo:
    ```
    git push
    ```

6. Create a pull request on github (the page of your fork will hint for your recently submitted branches)


#Project infrastructure
The leksah application consists of three different projects: `leksah`, `leksah-ide` and `ltk`. It uses the [gtk](http://hackage.haskell.org/package/gtk3) gui library which are direct bindings to the C library.

#Types

This section summarizes some of the most important types in the code base. Most types are defined in [src/IDE/Core/Types.hs](https://github.com/leksah/leksah/blob/master/src/IDE/Core/Types.hs).


## General application state
These are some of the important types that describe (parts of) the application state.

===

###[`IDE`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:IDE)
The application's state, such as the user preferences, the list of current errors and the open workspace. Operated on by the `IDEM` monad.

===

###[`IDEPackage`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:IDEPackage)
Contains the information of a cabal package

===

###[`Workspace`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:Workspace)
Contains all the information of a leksah workspace.

===

###[`Prefs`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:Prefs)
The user preferences



## Actions in the IDE

The Leksah codebase uses a few custom monads to work conveniently in the context of the `IDE`, `Workspace` or an `IDEPackage`.

===

###[`IDEM a`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:IDEM)
The IDE monad, it maintains a state of type `IDE`. Actions that happen at the level of the IDE should use this type. For example, `readIDE recentFiles :: IDEM [FilePath]` gets the list of recent files, `showWorkspace :: IDEM ()` shows the workspace pane.

Implemented as `type IDEM = ReaderT (IORef IDE) IO`

*Relevant types and functions*
```haskell
type IDEAction = IDEM ()
readIDE    :: (IDE -> a) -> IDEM a
modifyIDE_ :: (IDE -> IDE) -> IDEM ()
liftIO     :: IO a -> IDEM a

-- Running an IDE action in IO (for instance when registering a callback in gtk)
reflectIDE :: IDEM a -> IORef IDE -> IO a

-- Lift an IO action with the `IDE` reference in scope
reifyIDE :: (IORef IDE -> IO a) -> IDEM a

-- Typeclass for any other monad that can run an `IDEM a` action
class (Functor m, Monad m, MonadIO m) => MonadIDE m where
    liftIDE :: IDEM a -> m a

instance MonadIDE IDEM where ...

ideMessage :: MonadIDE m => MessageLevel -> Text -> m ()
```

===

### [`WorkspaceM a`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:WorkspaceM)

Used when writing IDE actions that work with the open workspace. This is implemented as `type WorkspaceM = ReaderT Workspace IDEM`. See [src/IDE/Workspaces.hs]() for examples.

*Relevant types and functions*
```haskell
type WorkspaceAction = WorkspaceM ()

-- When no workspace open, prompts the user to open one
workspaceTry :: WorkspaceAction -> IDEAction
-- when no workspace open, fails with a message in the log pane
workspaceTryQuiet :: WorkspaceAction -> IDEAction
ask :: WorkspaceM Workspace
liftIDE :: IDEM a -> WorkspaceM a
```

===

### [`PackageM a`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:PackageM)

Used when writing IDE actions that work with a specific package, in a specific workspace. This is implemented as `ReaderT IDEPackage WorkspaceM`. See [src/IDE/Package.hs](https://github.com/leksah/leksah/blob/master/src/IDE/Package.hs) for examples.

*Relevant types and functions*
```haskell
type PackageAction = PackageM ()

-- When no package is active, prompts the user to activate one
packageTry :: PackageAction -> IDEAction

-- When no package is active, fails with a message in the log pane
packageTryQuiet
liftIDE :: IDEM a -> PackageM a
```

===

### [`DebugM a`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:DebugM)

Used for actions that interact with ghci for a specific package. This is implemented as `type DebugM = ReaderT (IDEPackage, ToolState) IDEM`. See [src/IDE/Debug.hs](https://github.com/leksah/leksah/blob/master/src/IDE/Debug.hs) for examples.

*Relevant types and functions*
```haskell
type DebugAction = DebugM ()

runDebug :: DebugM a -> (IDEPackage, ToolState) -> IDEM a

-- When ghci is not running, prompts the user to activate ghci
tryDebug :: DebugAction -> PackageAction

-- When ghci is not running, fails quietly
tryDebugQuiet :: DebugAction -> PackageAction
```


## Events

Leksah has a mechanism for triggering and handling events.

===

### [`IDEEvent`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:IDEEvent)

All the events that can be triggered. When adding a new event, also adjust the instance of `Event` and `EventSource` (both in [src/IDE/Types.hs](https://github.com/leksah/leksah/blob/master/src/IDE/Core/Types.hs)).

*Relevant types and functions*
```haskell
triggerEventIDE :: MonadIDE m => IDEEvent -> m IDEEvent
registerEvent :: IORef IDE -> Text -> (IDEEvent -> IDEAction) -> IDEAction
```

## Action Descriptions

### [`ActionDescr`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:ActionDescr)
An action description is a small abstraction layer over GtkActions, which are used for menus, toolbars, and keyboard shortcuts. The structure contains info about tooltip text, menu icon etc. See [src/IDE/Command.hs](https://github.com/leksah/leksah/blob/master/src/IDE/Command.hs) for the list of all available action descriptions.


### [`SensitivityMask`](http://hackage.haskell.org/package/leksah/docs/IDE-Core-Types.html#t:SensitivityMask)
A `SensitivityMask` covers a certain set of action descriptions of which the corresponding gtk components (tool buttons, menu entries, shortcut keys) should be simultaneously sensitive (enabled) or insensitive (disabled). Fore example, when the text editor has no focus commands such as "FileClose" and "Goto Line" are set to insensitive.

*Relevant types and functions*
```haskell
setSensitivity :: [(SensitivityMask, Bool)] -> IDEAction
```

## Autocompletion

TODO

## Metadata

TODO

## Panes

The `ltk` library defines a set of typeclasses for panes. For every pane in the IDE there is a seperate file in [src/IDE/Pane/](https://github.com/leksah/leksah/tree/master/src/IDE/Pane) which implements these.

===

### [`class Pane p m`](http://hackage.haskell.org/package/ltk/docs/Graphics-UI-Frame-Panes.html#t:Pane)
An instance of this typeclass defines the functions for identifying the pane of type `p`, operated on in monad `m` (all current panes use `IDEM`). Most panes consist mainly of some data and some GTK widgets.

===

### [`class RecoverablePane p s m`](http://hackage.haskell.org/package/ltk/docs/Graphics-UI-Frame-Panes.html#t:RecoverablePane)
An instance of this typeclass can restore the pane representation (of type `p`) after restarting the IDE. Every pane has a piece of state (of type `s`) that will be saved to disk and reloaded on startup. For instance, the pane for the text editor uses the following type to save its state:

```haskell
-- | State for a buffer that points to a file on disk,
-- or an unsaved file
data BufferState            =   BufferState
                                    FilePath -- ^ The opened file
                                    Int      -- ^ Cursor position (amount of characters)
                            |   BufferStateTrans
                                    Text     -- ^ The buffer name
                                    Text     -- ^ The text in the buffer
                                    Int      -- ^ Cursor position (amount of characters)
    deriving(Eq,Ord,Read,Show,Typeable)
```

Another example is the Symbol info pane ([src/IDE/Info.hs](https://github.com/leksah/leksah/blob/master/src/IDE/Pane/Info.hs)) which remembers the symbol it was displaying.

Furthermore, an instance defines `builder` which should construct a pane.

*Relevant types and functions*
```haskell
-- (simplified types with m ~ IDEM)

-- Tries to get "the" pane of type p, in general there can only be one
-- pane per type with the exception of text buffers, for which it tries to
-- return the first.
getPane   :: RecoverablePane p st IDEM => IDEM (Maybe p)

displayPane :: RecoverablePane p st IDEM
            => p         -- ^ The pane
            -> Bool      -- ^ Whether it should also grab the focus
            -> IDEAction
closePane :: RecoverablePane p st IDEM => p -> IDEM Bool
```


### [data IDEBuffer](https://github.com/leksah/leksah/blob/master/src/IDE/BufferMode.hs#L57)

This pane contains a text editor, it's confusingly located in `src/BufferMode.hs` while some of its instances are located in `src/Pane/SourceBuffer`.

```haskell
data IDEBuffer = forall editor. TextEditor editor => IDEBuffer {
...
,   sourceView      ::  EditorView editor
...
} deriving (Typeable)
```

## Text editors
There exist currently three different editor backends:


| Backend | Source | Description|
|---|---|---|
|**GtkSourceView** (default) | `src/IDE/TextEditor/GtkSourceView.hs` | A [C library](https://developer.gnome.org/gtksourceview/stable/) for which there are [Haskell bindings](). This is the default, stable editor which has most features implemented of the three. Syntax highlighting styles are in `xml` format, specific to GtkSourceView, found in `data/styles`. Language specification files are in `xml` format, specific to GtkSourceView, found in `language-specs`|
|**CodeMirror** (unstable)| `src/IDE/TextEditor/CodeMirror.hs` | [JavaScript library](https://codemirror.net/) for in the browser which is implemented with the Haskell [JSaddle library](http://hackage.haskell.org/package/jsaddle). Configure leksah with the `codemirror` cabal flag like so and rebuild:`cabal configure -fcodemirror`. Then change the text editor in the leksah Preferences window|
| **Yi** (unstable) |`src/IDE/TextEditor/Yi.hs`| A text editor implemented in Haskell. Build leksah with the `yi` and `dyre` flags, change the editor in the leksah Preferences window `cabal configure -fyi -fdyre`. Then change the text editor in the leksah Preferences window|

All backends implement the [`TextEditor`](https://github.com/leksah/leksah/blob/master/src/IDE/TextEditor/Class.hs#L57) typeclass, which defines a common interface for text editors:

```haskell
class TextEditor editor where
    data EditorBuffer editor
    data EditorView editor
    data EditorIter editor
    ...

```
The names of the associated types are based on those of Gtk Text Widget ([conceptual overview](https://developer.gnome.org/gtk3/stable/TextWidget.html)) as the type class is very much biased towards the GtkSourceView implementation:

* `EditorBuffer editor` represents the text being edited
    * Some related class methods:

      ```haskell
      getModified :: EditorBuffer editor -> IDEM Bool
      getLineCount :: EditorBuffer editor -> IDEM Int
      ```
* `EditorView editor` is a widget that represents the GTK widget
    * The corresponding buffer of a view is obtained by `getBuffer :: EditorView editor -> IDEM (EditorBuffer editor)`
    * Some related class methods:

      ```haskell
      setShowLineNumbers :: EditorView editor -> Bool -> IDEM ()
      setTabWidth :: EditorView editor -> Int -> IDEM ()
      afterMoveCursor :: EditorView editor -> IDEM () -> IDEM [Connection]
      ```
* `EditorIter editor` is a location in the source buffer
   * Some related class methods:

     ```haskell
     getSlice :: EditorBuffer editor
                -> EditorIter editor
                -> EditorIter editor
                -> Bool
                -> IDEM Text
     selectRange :: EditorBuffer editor -> EditorIter editor -> EditorIter editor -> IDEM ()
     ```


## Other terms

| Term | Description|
|-----|----|
| Pango | Pango is the core text and font handling library used in GNOME applications. It has extensive support for the different writing systems used throughout the world.|
|GTK+|GTK+ is the primary library used to construct user interfaces in GNOME applications. It provides user interface controls and signal callbacks to control user interfaces.|
|GDK|An intermediate layer which isolates GTK+ from the details of the windowing system.|
|Cairo| Cairo is a 2D graphics library with support for multiple output devices. It is designed to produce consistent output on all output media while taking advantage of display hardware acceleration when available.|
|Webkit2GTK|Web content rendering for the GNOME Platform|
