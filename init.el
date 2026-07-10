;; -*- lexical-binding: t -*-

;; Wakib Emacs -- a modern, user-friendly Emacs starter kit.
;; This file only orchestrates: package setup, then the modules below.

(when (< emacs-major-version 30)
  (error "Wakib Emacs requires Emacs 30 or later (you are running %s), see README.md"
	 emacs-version))

;; -------------------
;; Packages
;; -------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Emacs only applies use-package-always-ensure once use-package-ensure
;; is loaded, so load it before the first use-package call
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;; Packages downloaded on first start are byte-compiled on the spot;
;; their compiler warnings concern package authors, not users, so keep
;; them out of *Compile-Log* while the kit bootstraps
(setq byte-compile-warnings nil)
(add-hook 'emacs-startup-hook (lambda () (setq byte-compile-warnings t)))
(use-package diminish)

;; -------------------
;; Load Path
;; -------------------
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "user" user-emacs-directory))

;; -------------------
;; Modules
;; -------------------
;; Order matters: wakib-core enables the wakib keymaps that later
;; modules attach bindings to.
(require 'wakib-core)       ; wakib-keys, undo, better built-in defaults
(require 'wakib-ui)         ; theme, menu bar, which-key, windows
(require 'wakib-completion) ; minibuffer and in-buffer completion
(require 'wakib-editing)    ; text editing tools, snippets, spelling
(require 'wakib-dev)        ; git, projects, LSP, tree-sitter

;; Machine-local settings written by Customize
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)

;; User overrides, loaded last so they win.
;; See refs/init-user.el.template for how to set up user/init-user.el
(require 'init-user nil t)
