# Wakib - Emacs For The Rest Of Us 

This is an Emacs starter kit project that aims to bring a modern,
user-friendly version of Emacs. This starter kit build off the bindings of
the [wakib-keys](https://github.com/darkstego/wakib-keys) project.
While that focused on the changing many of the default keybindings in
emacs, this starter kit builds on it by adding changes to the look
and behaviour of Emacs, as well as bundling many of the very useful packages.

The purpose of this is to make an Emacs starter kit that "Just Works".
If you are interested in all the power of Emacs without the steep
learning curve, then this is the startr kit for you.  Hopefully, this
will make Emacs a viable option even for someone just starting to
learn programming.

You can find a video introduction about Wakib [here](https://youtu.be/rK51Lp_lreI).

## Features

Many of the features of Wakib were designed to  prioritizes the user experience. These include: 

* Use of Common shortcuts. No need to learn new ways to Open, Save, Copy, Paste...
* Consistentcy. The shortcuts are displayed correctly in the menu bar and minibuffer.
* New buffers defaults to Org mode. One of the best things about Emacs
  should feature front and center.
* Efficient. Like Vim and Emacs bindings, Wakib was designed to keep
  your fingers near the homekeys for all common commands.
* Ergonomic. The most repetitive commands use Alt instead of Ctrl to reduce RSI.
* Easy to learn. Shortcuts are grouped together logically to enable
  users to quickly pick up keybindings.


## Installation

You must, of course, have installed Emacs on your system first.

To install this starter kit, clone this repo to your user emacs
directory (typically `~/.emacs.d`).  If you would like to save your
old configuration make sure to back up your user emacs directory by
moving or renaming it.

On Linux and MacOS the installation is simply
```
git clone https://github.com/darkstego/wakib-emacs.git ~/.emacs.d
```

In the case of Windows OS then you can simply run the following in git bash
```
git clone https://github.com/darkstego/wakib-emacs.git ~/AppData/Roaming/.emacs.d 
```

The first time emacs starts after this, it will automatically download all 
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

### C-e ? (Extended Shortcuts formerly C-x)
This points to the typical C-x map in Emacs. The main purpose in Wakib
is to provide room for extra shortcuts. Any of the old Emacs C-x
shortcuts that now have equivalent as a common shortcut can be
recycled.

### C-d C-? (Mode Shortcuts formerly C-c)
This points to the C-c map in Emacs. Major modes typically populate
their shortcuts here, and these are untouched in wakib.

### C-d ?  (User Shortcuts)
As per the Emacs guidlines, keybindings that start with C-d and a single
character should be reserved for user, so as a general rule these will not
be populated. This allows users to add their own custom shortcuts knowing
that they won't be overwritten by future updates to Wakib.


## User local changes

In order to add your own changes to the starter kit that won't be
overwritten anytime you update the project, create a subdirectory
named `user` in your emacs.d directory, and add your changes to a file
called `init-user.el`. You could just copy or rename the template from
the `refs/init-user.el.template` file that comes with the project. The
template file contains extra configuration that while useful, did not
make sense as default behaviour for the starter kit.

If you would like to use some of your own keybindings instead of the
ones provided by wakib, you have two ways to do so:

1. If you are overwriting a key to do something totally different from
   the original, then just overwrite the key in
   `wakib-keys-overriding-map` so for example: `(define-key
   wakib-keys-overriding-map (kbd "C-q") 'quoted-insert)`

2. If you are remapping a certain function and have all keys that do
   that function do something else instead then just apply remaps to
   the global-map or any active mode, for example: `(define-key
   (current-global-map) [remap undo] 'my-undo)`

## Contribution

Be it code, bugfixes, or just a suggestion of a behavior or package
that should be added to this config. Just open up an issue on
github. I am really interested to get other opinions on what is
working and what isn't.
