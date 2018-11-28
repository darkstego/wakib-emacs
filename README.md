# Wakib.d - Easy, Powerful Emacs 

This is an Emacs starter kit project that aims to bring a modern,
user-friendly version of Emacs. This starter kit build off the bindings of
the [wakib-keys](https://github.com/darkstego/wakib-keys) project.
While that focused on the changing many of the default keybindings in
emacs, this starter kit builds on it by adding changes to the look
and behaviour of Emacs, as well as bundling many of the very useful packages.

The purpose of this is to make an Emacs starter kit that "Just Works".
This isn't meant to be a copy of my entire emacs.d, but rather a bunch
of sensible defaults that would allow users to hit the ground running
with Emacs. This also allows users to customize their own experience
with private changes from using this starter kit as a base.


## Installation

To install, clone this repo and its submodules to  `~/.emacs.d`.
If you would like to save your old configuration make sure to
back up your `~/.emacs.d` by moving or renaming it.

```
git clone https://github.com/darkstego/wakib.d.git ~/.emacs.d
```
The first time emacs starts after this, it will automaticall download all 
the third-party packages.

## Bindings

CTRL
-----
![CTRL-KEYS](https://s15.postimg.cc/9bmeocmqz/Keyboard_CTRL_Layout.png "wakib-keys ctrl bindings")

ALT
---
![ALT-KEYS](https://i.postimg.cc/Fz0qq6DQ/Keyboard-ALT-Layout3.png "wakib-keys alt bindings")

## Binding Policy (AKA Where are my C-c, C-x keys)

In order to properly implement copy and cut using the standard C-c and
C-x keys, the old Emacs prefix keys needed to be moved to another key
(C-c became C-d, and C-x became C-e). The keybinding system used in
Wakib are as follows

### Alt (Nav/Sel/Del)
These shortcuts mainly have to do with navigating and deleting/selecting
text. These are more ergonomically friendly than using the Ctrl key,
so commands that are commonly repeated or used very frequently should also end up here.

### Ctl (Common Shortcuts)
Commands that are very common end up here. Most of these shortcuts are
the common shortcuts users have grown accustomed to. This can be also
used for commands that need to be fast and concise.

### C-e ? (Extended Shortcuts)
This points to the typical C-x map in Emacs. The main purpose in Wakib
is to provide room for extra shortcuts. Any of the old Emacs C-x
shortcuts that now have equivalent as a common shortcut can be
recycled.

### C-d C-? Mode Shortcuts
This points to the C-c map in Emacs. Major modes typically populate
their shortcuts here, and these are untouched in wakib.

### C-d ?  User Shortcuts
As per the Emacs guidlines, keybindings that start with C-d and a single
character should be reserved for user, so as a general rule these will not
be populated. This allows users to add their own custom shortcuts knowing
that they won't be overwritten by future updates to Wakib.




## User local changes

In order to add your own changes to the starter kit that won't be
overwritten anytime you update the project, create a subdirectory
named `local` in your emacs.d directory, and add your changes to a
file called `init-local.el`. You could just copy or rename the
template from the `init-local.el.template` file that comes with the
project. The template file contains some common changes that did not
fit as part of the default starter kit.


## Contribution

Be it code, bugfixes, or just a suggestion of a behavior or package
that should be added to this config. Just open up an issue on
github. I am really interested to get other opinions on what is
working and what isn't.
