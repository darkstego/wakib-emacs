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
![ALT-KEYS](https://s15.postimg.cc/407i3k0nf/Keyboard_ALT_Layout.png "wakib-keys alt bindings")


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
