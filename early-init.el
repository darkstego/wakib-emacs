;; -*- lexical-binding: t -*-

;; Loaded before the GUI and the package system are initialized.
;; Keep this file limited to frame and startup settings; all real
;; configuration belongs in init.el and modules/.

;; Defer garbage collection during startup, restore a sane limit after
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 32 1024 1024))))

;; Frame settings applied before the first frame is created.
;; The menu bar stays on: Wakib menus display the correct shortcuts,
;; which is a core part of making Emacs discoverable for beginners.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-screen t)
(setq load-prefer-newer t)              ; Don't load outdated byte code
