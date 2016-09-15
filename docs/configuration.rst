Configuration
=============

Leksah is highly customizable and can be adapted to your specific needs
and work organization. What follows here is not needed for initial use
of Leksah (and need not be read on a first lecture of the manual).
Leksah works well with the default settings and a desire to adapt better
to your work habits comes only with extended use of Leksah. However,
with time, you may use one or the other option to tailor Leksah to your
personal preference. It is easy! Here it is explained how this works.

Layout
------

O0.3

|image|

|image|

|image|

Leksah has always one special pane, which is called the active pane, and
its name is displayed in the second compartment from the left side in
the status bar. Some actions like moving, splitting, closing panes or
finding or replacing items in a text buffer act on the current pane, so
check the display in the status bar to see if the pane you want to act
on, is really the active one.

You can tailor the layout with the View menu to suit your work style
better. Internally, the panes are arranged in a layout of a binary tree,
where the leaves are horizontal or vertical splits. Every area can be
split horizontally or vertically and panes can collapse. With the
commands in the View menu you manipulate this tree to change the layout.

In the initial pane positions part of the Preferences, you can configure
the placement of panes. Panes belongs to categories, and a category
specify a path were a pane will open .

The layout of the Leksah window contains areas which contain notebooks
which contain panes. The division between the two areas is adjustable by
the user by dragging a handle.

Panes can be moved between areas in the window. This can be done by
dragging the notebook tab, and release it on the frame of another
notebook. Alternatively you can use keystrokes (Shift Alt Arrow) to move
panes around. The tabs of notebooks can be positioned at any of the four
directions, or the tabs can be switched off.

Note that holding the mouse over the tabs and selecting the right button
brings up a menu of all panes in this area, so that you can for example
quickly select one of many open source buffers.

The layout will be saved with sessions. The session mechanism will be
explained in [sub:Session-handling]. [1]_

Advanced layout: Group panes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A notebook cannot only contain single panes, but it can as well contain
group panes, which have a layout on their own and may contain arbitrary
other panes; the debug pane is an example for a group pane. This gives
you the possibility to arrange the subpanes in a debugger pane as it
fits you best.

A new group starts by selecting View / Group \ from the menu. You have
to give a unique name for the group. Then you can arrange panes in the
group as you like. When closing a group, and the group is not empty, you
have to confirm it.

Using Leksah with multiple displays: Detached windows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This feature allows you to move panes to a separate window on a separate
display. This is as simple as: You select a notebook and choose View /
Detach from the menu bar. Then the notebook is opened in a new window,
which you can then move to another screen.

When you close the detached window, the pane goes back to the place
where it was before detaching. The state of detached window is
remembered, when you close Leksah, and they will be reopened when you
restart Leksah.

It is possible to drag and drop panes between windows. But splitting and
collapsing of panes is disabled for detached windows. So a recommended
way to use this feature is to split a pane, arrange the panes that you
want to detach in the area of the new notebook. Select the new notebook
and detach.

The detached windows have no menu bar, toolbar and status bar on their
own. This may be a problem, when you want to select a menu entry: the
focus changes from a pane in the detached window to a pane in the main
window, and you may not be able to do what you want. We recommend that
you use keystrokes or context menus.

[sub:Session-handling]Session handling
--------------------------------------

When you close Leksah the current state is saved in the file
*current.lksks* in the *~/.leksah-\*.\** folder. A session contains the
layout of the window, its content, the active package and some other
state. When you restart Leksah it recovers the state from this
information.

When you close a workspace, the session is saved in the folder of the
workspace *in* a file named *workspacename.lksks*. When you open a
workspace and Leksah finds a *workspacename.lksks* file together with
the workspace file you are going to open, you get prompted if you want
to open this session (this means mostly opening the files you had open
before in the editor). This helps you to switch between different
workspaces you are working on.

In addition, sessions can be stored and loaded with a name manually by
using the session menu, but the need to use these features occurs
rarely. The menu Configuration -> Forget Session is useful if you
inadvertently changed the layout drastically and do not want the current
session to be stored.

[sub:Shortcuts]Shortcuts
------------------------

You can configure the keystrokes by providing a keymap file, which
should be be in the ~/.leksah-0.8 folder. The name of the key map file
to be used can be specified in the Preferences dialog (without
extension!).

A line in the .keymap file looks like:

<ctrl>o -> FileOpen Opens an existing file

Description of the key or key combination: Allowed modifiers are <shift>
<ctrl> <alt> <apple> <compose>. <apple> is on a Microsoft keyboard the
windows key (and on a Mac, obviously, the apple key!). <compose> is
right ALT key, often labeled Alt Gr. It is as well possible to specify
Emacs like keystrokes in the following way: <ctrl>x/<ctrl>f -> FileOpen
Opens an existing file

The name of the action can be any one of the *ActionDescr’s* given in
the *action* function in the Module *IDE.*\ Command. The comment
following will be displayed as tool tip for the toolbar button, if one
exists for this action.

Every keystroke must at most be associated with one action, and every
action may only have one associated keystroke.

Simple keystrokes are shown in the menu, but Emacs like keystrokes are
not. This is because simple keystrokes are handled by the standard GTK
mechanism, while other keystrokes are handled by Leksah.

Independently how you initiated an action, by a menu, a toolbar button
or a keystroke, the keystroke with its associated ActionsString is
displayed in the Status bar in the leftmost compartment.

Configuration files
-------------------

Leksah stores its configuration in a directory called ~/.leksah-\*.\*
under your home folder. Indexing the hidden Leksah directory with the
version number avoids that changes to preferences file layout from
version to version cause difficulties. Moving your preferences from a
previous version can potentially be automatic.

The file *prefs.lkshp* stores the general preferences. It is a text file
you could edit with a text editor, but more comfortable and safer is to
do it in Leksah with the menu Configuration / Edit Prefs from the menu.

If no preference file is found in your .leksah-\*.\* folder then the
global prefs.lkshp in the installed data folder will be used. If a
preference file get corrupted, which means Leksah does not start; it is
then often sufficient to just delete the preference file.

The source\_packages.txt file stores source locations for installed
packages. It can be rebuild by calling leksah-server in a terminal with
the -o or –sources argument . Do this after you moved your source or
added sources for previous installed packages without sources.

Files for Keymaps (keymap.lkshk) and SourceCandy(candy.lkshc) may be
stored in the ~/.leksah-\*.\* folder and will be found according to the
name selected in the Preferences Dialog. Leksah first searches in this
folder and after this in the /data folder.
