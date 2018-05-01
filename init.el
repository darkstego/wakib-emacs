;; -*- lexical-binding: t -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(setq debug-on-error t)

(nconc load-path
       (list (expand-file-name "local" user-emacs-directory)
	     (expand-file-name "wakib" user-emacs-directory)))


;; TODO move wakib to global keymap
(require 'wakib-mode)
(wakib-mode 1)


;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
;;(let ((normal-gc-cons-threshold (* 20 1024 1024))
;;      (init-gc-cons-threshold (* 128 1024 1024)))
;;  (setq gc-cons-threshold init-gc-cons-threshold) 
;;  (add-hook 'after-init-hook		
;;            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
(setq gc-cons-threshold (* 20 1024 1024))


;; -----------------------
;; use-package
;; -----------------------
(setq load-prefer-newer t)              ; Don't load outdated byte code

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)


(use-package diminish)

;; -------------------
;; Theme
;; -------------------
(use-package moe-theme
  :config
  (load-theme 'moe-dark t))

;; -------------------
;; Magit
;; -------------------
(use-package magit
  :bind
  (("C-x g" . magit-status )))

;; -------------------
;; Ivy
;; -------------------
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format ""))

(use-package counsel
  :diminish counsel-mode
  :disabled
  :config
  (counsel-mode 1))

;; -------------------
;; Projectile
;; -------------------
;; No deferred loading as bind-keymap
;; doesn't handle wakib C-d keymaps
(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))

;; -------------------
;; expand-region
;; -------------------
(use-package company               
  :diminish company-mode
  :config
  (global-company-mode 1))

;; -------------------
;; expand-region
;; -------------------
(use-package expand-region
  :commands er/expand-region
  :bind ("C-S-a" . er/expand-region))

;; -------------------
;; uniquify
;; -------------------

;; TODO (built into emacs, check performance hit if worth it)


(use-package quickrun
  :config
  (setq quickrun-focus-p nil)
  :bind
  (([f8] . quickrun )))


;; Better Parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(show-paren-mode 1)
(electric-pair-mode 1)
;; TODO - MOVE Electric Pair Mode to user local


;; MAJOR MODES

(use-package markdown-mode
  :mode "\\.\\(m\\(ark\\)?down\\|md\\)$")



;; Setup Splash Screen
(setq inhibit-startup-screen t)
(setq-default major-mode 'org-mode)
(setq initial-buffer-choice
      (lambda ()
	(let ((buffer (generate-new-buffer "untitled")))
	  (set-buffer-major-mode buffer)
	  buffer)))
(setq-default initial-scratch-message ";; Emacs elisp scratch buffer. Happy hacking.\n\n")


(tool-bar-mode -1)
(delete-selection-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(custom-safe-themes
   (quote
    ("291588d57d863d0394a0d207647d9f24d1a8083bb0c9e8808280b46996f3eb83" "eecacf3fb8efc90e6f7478f6143fd168342bbfa261654a754c7d47761cec07c8" default)))
 '(package-selected-packages
   (quote
    (rebinder ivy-historian package-lint dracula-theme moe-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
