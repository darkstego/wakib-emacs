<h2 align="center"><img src="https://user-images.githubusercontent.com/2610287/236253542-ab75c56d-0bbc-457a-8587-d5d2c78d0eb3.svg" height="128"><img src="https://www.gnu.org/software/emacs/images/emacs.png" height="128"><br>Wakib: Emacs for the rest of us</h2>

This is an Emacs starter kit that aims to bring a modern,
user-friendly version of Emacs. It builds on the keybindings of the
[wakib-keys](https://github.com/darkstego/wakib-keys) project. While
that project focused on changing the default keybindings of Emacs,
this starter kit adds changes to the look and behaviour of Emacs, and
bundles the best packages of the Emacs ecosystem with sensible
defaults.

The purpose of this is to make an Emacs starter kit that "Just Works".
If you are interested in all the power of Emacs without the steep
learning curve, then this is the starter kit for you. Hopefully, this
will make Emacs a viable option even for someone just starting to
learn programming.

You can find a video introduction about Wakib [here](https://youtu.be/rK51Lp_lreI).

If you like these keybindings checkout the [Wakib project](https://github.com/darkstego/wakib-project) to use these bindings outside of Emacs.

## Core Principles

* **Low friction.** The shortcuts you use everywhere (Open, Save,
  Copy, Paste, Undo...) work here too, so no steep learning curve.
* **Fast.** Starts up quickly and stays snappy.
* **Sensible Defaults.** Using some of the most well regarded tools in
  the Emacs ecosystem like Magit, Vertico, Corfu, tree-sitter,
  Eglot... set up with sensible defaults so you don't have to.
* **Editor to IDE.** Use it as a quick text editor, or use it as a
  full IDE (completion, diagnostics, jump-to-definition) when you
  need it.
* **Beginner friendly.** Menus show the correct shortcuts, prompts
  annotate themselves, and a which-key popup teaches you the rest.
* **Small.** A slim init and a few small modules, built on
  what already ships with Emacs wherever possible.
* **User Expandable.** User modification live in their own folder
  and can be stored in their own git repo.

## Requirements

**Emacs 30 or later.** The starter kit leans on features that are built into
modern Emacs (use-package, eglot, tree-sitter, which-key,
project.el), which keeps it fast and dependable.

## Installation

To install this starter kit, clone this repo to your user emacs
directory (typically `~/config/emacs` or `~/.emacs.d`). If you would like to save your
old configuration make sure to back up your user emacs directory by
moving or renaming it.

On Linux and MacOS the installation is simply

```
git clone https://github.com/darkstego/wakib-emacs.git ~/config/emacs
```

In the case of Windows OS then you can simply run the following in git bash

```
git clone https://github.com/darkstego/wakib-emacs.git ~/AppData/Roaming/.emacs.d
```

The first time emacs starts after this, it will automatically download all
the third-party packages.

## Updating

To update the starter kit, pull the latest version from inside your
emacs directory:

```
git pull
```

Updates never touch your `user/` directory or `custom.el`, so your
own configuration and customizations are safe. To also update the
third-party packages, run `M-x package-upgrade-all` inside Emacs.

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

## Using Emacs as an IDE

* **Language support.** Opening a source file offers to install its
  tree-sitter grammar (needs a C compiler; answer `n` and Emacs falls
  back to the classic mode).
* **Language server.** Run `M-x eglot` (or Tools → Start Language
  Server) in a project to get completion, documentation and
  diagnostics. You need the language server for your language
  installed; see the [Eglot manual](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html)
  for the list. To start it automatically for your languages, see
  `refs/init-user.el.template`.
* **Projects.** `C-e p` is the project prefix: find file (`C-e p f`),
  search with ripgrep (`C-e p g`), switch project (`C-e p p`). A
  Project menu appears in the menu bar whenever you are in one.
* **Git.** `C-e g` opens Magit. Uncommitted changes are highlighted
  in the fringe.
* **Run code.** `F8` runs the current buffer with quickrun.

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

## What's Inside

| Area | Packages |
|------|----------|
| Keybindings | [wakib-keys](https://github.com/darkstego/wakib-keys), which-key |
| Minibuffer completion | vertico, orderless, marginalia, consult, embark |
| In-buffer completion | corfu, cape |
| Git | magit, diff-hl |
| Projects | project.el |
| IDE | eglot, tree-sitter via treesit-auto, flymake, quickrun |
| Languages | everything built into Emacs, plus markdown, yaml, go, rust, lua, typescript, dockerfile, cmake |
| Editing | avy, expand-region, multiple-cursors, yasnippet, vundo |
| Looks | modus-vivendi-tinted theme, rainbow-delimiters |


## Breaking Changes (2.0.0 redesign)

If you are upgrading from an older Wakib Emacs, note the following:

* **Emacs 30+ is now required.** Older versions stop with an error at
  startup.
* **Delete your old packages when upgrading in place**: run
  `rm -rf ~/.emacs.d/elpa` before the first start so stale packages
  don't shadow the ones now built into Emacs.
* **Ivy/Counsel/Company were replaced by Vertico/Consult/Corfu.**
  Day-to-day muscle memory carries over: `C-b` switches buffers,
  `M-x` completes commands, `M-i`/`M-k` and `M-;`/`M-:` move through
  candidates, Tab completes in buffers and Return still inserts a
  newline.
* **undo-tree was replaced.** `C-z`/`C-S-z` are undo/redo (built-in),
  and `C-e u` opens vundo, a visual undo tree, when you need to
  recover an old branch of history.
* **Projectile was replaced by the built-in project.el.** The prefix
  is still `C-e p` and the common commands (`f`, `p`, `g`) are the
  same, but projectile-specific commands are gone.
* **The theme is now modus-vivendi-tinted** (built-in). To get a
  different look, load any theme from `user/init-user.el`.
* **New buffers still default to Org mode**, and Company's
  tab-to-complete behaviour was carried over to Corfu.

## Changelog

* Complete redesign: Emacs 30+, modular config (`modules/`),
  early-init.el for faster startup, Vertico/Consult/Corfu completion,
  built-in project.el, eglot and tree-sitter support. See Breaking
  Changes above.
* Company no longer uses *return* for completion, but rather uses tab
  to complete part, and tab again to select. This solves the problem
  of not being able to insert a newline because Company mode opened
  an autocomplete popup.

## Contribution

Be it code, bugfixes, or just a suggestion of a behavior or package
that should be added to this config. Just open up an issue on
github. I am really interested to get other opinions on what is
working and what isn't.
