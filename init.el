;; -*- lexical-binding: t -*-

;;----------------------------------------------------------------------------
;; Adjust garbage collection
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 20 1024 1024))


;; ---------------------
;; Setup Load Path
;; ---------------------

(add-to-list 'load-path (expand-file-name "user" user-emacs-directory))


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
;; Fix tls bug
(when (< (string-to-number emacs-version) 26.3)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(use-package diminish)

;; -------------------
;; Wakib
;; -------------------
(use-package wakib-keys
  :diminish wakib-keys
  :config
  (wakib-keys 1)
  (add-hook 'after-change-major-mode-hook 'wakib-update-major-mode-map)
  (add-hook 'menu-bar-update-hook 'wakib-update-minor-mode-maps)
  ;; Modifying other modules
  ;; When remap is used it exits isearch abruptly after first instance
  ;; Use explicit keybindings instead
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "M-;") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-:") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "M-d") 'isearch-delete-char))

 
;; -------------------
;; Initial Setup
;; -------------------
(tool-bar-mode -1)
(unless (display-graphic-p)
  (menu-bar-mode -1))

(cua-selection-mode 1)
(define-key cua-global-keymap (kbd "<C-return>") nil)
;;(define-key cua--rectangle-keymap (kbd "ESC") nil)
;;(define-key cua-global-keymap (kbd "<C-return>") nil)
;;(define-key cua-global-keymap (kbd "C-x SPC") 'cua-rectangle-mark-mode)

(define-key read-expression-map [remap previous-line] 'previous-line-or-history-element)
(define-key read-expression-map [remap next-line] 'next-line-or-history-element)

(setq org-export-with-toc nil)
(setq org-support-shift-select t)
(setq frame-title-format '((:eval (buffer-name))" [%+] Wakib Emacs"))

;; Menu Bars
;; TODO - Change bind-key to define-key
(bind-key [menu-bar file new-file]
	  `(menu-item "New File..." wakib-new-empty-buffer :enable (menu-bar-non-minibuffer-window-p)
		      :help "Create a new blank buffer"
		      :key-sequence ,(kbd "C-n")))

(bind-key [menu-bar file open-file]
	  `(menu-item "Open File..." find-file :enable (menu-bar-non-minibuffer-window-p)
		      :help "Read an existing or new file from disk"
		      :key-sequence ,(kbd "C-o")))

(bind-key [menu-bar file dired]
	  `(menu-item "Open Directory..." dired :enable (menu-bar-non-minibuffer-window-p)
		      :help "Browse a directory, to operate on its files"
		      :keys "C-e d"))

(bind-key [menu-bar file insert-file]
	  `(menu-item "Insert File..." insert-file :enable (menu-bar-non-minibuffer-window-p)
		      :help "Insert another file into current buffer"
		      :keys "C-e i"))

(global-unset-key [menu-bar options cua-mode])

;; Disable backup and lockfile
;; I hate browsing directories and seeing 'Emacs was Here' everywhere
(setq create-lockfiles nil
      make-backup-files nil)



;; -------------------
;; Theme
;; -------------------
(use-package moe-theme
  :config
  (load-theme 'moe-dark t)
  ;; diff-hl
  (custom-theme-set-faces
   'moe-dark
   '(diff-hl-insert ((t (:background "#008700" :foreground "#a1db00"))))
   '(diff-hl-change ((t (:background "#005f87" :foreground "#1f5bff"))))
   '(diff-hl-delete ((t (:background "#a40000" :foreground "#ef2929"))))
   '(flyspell-duplicate ((t (:underline (:color "#ff0000" :style wave)))))
   '(flyspell-incorrect ((t (:underline (:color "#ff0000" :style wave)))))))

;; -------------------
;; Undo-tree
;; -------------------
(use-package undo-tree
  :diminish undo-tree-mode
  ;; load undo tree or else won't start until redo triggered
  :defer 2
  :config
  (global-undo-tree-mode)
  (define-key undo-tree-visualizer-mode-map [remap left-char] 'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-mode-map [remap right-char] 'undo-tree-visualize-switch-branch-right)
  (setq undo-tree-auto-save-history nil)
  :bind
  (("C-S-z" . undo-tree-redo)))

;; -------------------
;; Magit
;; -------------------
(use-package magit
  :bind
  (("C-x g" . magit-status )))



(use-package exec-path-from-shell
  :disabled
  :config
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
    

;; -------------------
;; Ivy
;; -------------------
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (define-key ivy-minibuffer-map [remap keyboard-quit] 'minibuffer-keyboard-quit)
;;  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  (define-key wakib-keys-overriding-map (kbd "C-S-v") 'counsel-yank-pop))

;; find out what ivy uses from smex
(use-package smex)

;; -------------------
;; Flyspell-correct
;; -------------------
(use-package flyspell-correct-popup
  :config
  (define-key popup-menu-keymap (kbd "M-;") 'popup-next)
  (define-key popup-menu-keymap (kbd "M-:") 'popup-previous)
  (define-key popup-menu-keymap (kbd "M-k") 'popup-next)
  (define-key popup-menu-keymap (kbd "M-i") 'popup-previous)
  (define-key flyspell-mouse-map [mouse-2] nil)
  (define-key flyspell-mouse-map [mouse-3] 'flyspell-correct-word)
  (defun wakib-next-more (&optional arg)
  "Correct previous word"
  (interactive "p")
  (cond ((and flyspell-mode
	      (or (wakib-find-overlays-specifying 'flyspell-overlay)
		  (save-excursion
		    (backward-word)
		    (wakib-find-overlays-specifying 'flyspell-overlay))))
	(flyspell-correct-wrapper))))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-popup))

;; -------------------
;; Projectile
;; -------------------
;; No deferred loading as bind-keymap
;; doesn't handle wakib C-d keymaps
(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") nil)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode 1)
  (wakib-update-menu-map (global-key-binding [menu-bar tools Projectile])
			 projectile-command-map "C-e p")
  (define-key wakib-keys-map [menu-bar project]
	    `(menu-item ,"Project" ,(global-key-binding [menu-bar tools Projectile])
			:visible (projectile-project-p)))
  (define-key wakib-keys-map [menu-bar project seperator1] `(menu-item ,"--" nil))
  (define-key wakib-keys-map [menu-bar project git] `(menu-item ,"Git ..." magit-status :keys "C-e g"))
  (global-unset-key [menu-bar tools Projectile]))

;; -------------------
;; Yasnippet
;; -------------------

(use-package yasnippet-snippets
  :defer t)

(use-package yasnippet
  :hook
  ((prog-mode . yas-minor-mode))
  :diminish yas-minor-mode
  :config
  (require 'yasnippet-snippets)
  (yas-reload-all)
  (define-key yas-keymap [remap wakib-next] 'yas-next-field)
  (define-key yas-keymap [remap wakib-previous] 'yas-prev-field))


(use-package ivy-yasnippet
  :bind ("C-y" . ivy-yasnippet))


;; -------------------
;; Company
;; -------------------
(use-package company               
  :diminish company-mode
  :config
  (global-company-mode 1)
  (define-key company-active-map [remap wakib-next] 'company-select-next)
  (define-key company-active-map [remap wakib-previous] 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil))

;; -------------------
;; expand-region
;; -------------------
(use-package expand-region
  :bind ("M-A" . er/expand-region))

;; -------------------
;; avy
;; -------------------
(use-package avy
  :bind ("M-m" . avy-goto-char))

;; -------------------
;; switch-window
;; -------------------
(use-package switch-window
  :bind ("M-H" . switch-window)
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-threshold 1))

;; -------------------
;; which-key
;; -------------------
(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (which-key-mode))

;; -------------------
;; multiple-cursors
;; -------------------
;; TODO - Advice CUA-keyboard-quit to quit mc and rrm
(use-package multiple-cursors
  :init
  (custom-set-variables `(mc/always-run-for-all ,t))
  :config
  (define-key mc/keymap [remap keyboard-quit] 'mc/keyboard-quit)
  (define-key rectangular-region-mode-map [remap keyboard-quit] 'rrm/keyboard-quit)
  ;;(custom-set-variables `(mc/always-run-for-all ,t))
  :bind
  (("M-S" . set-rectangular-region-anchor)
   :map wakib-keys-overriding-map
	("C-." . mc/mark-next-like-this)
	("C-," . mc/mark-previous-like-this)
	("<C-down-mouse-1>" . mc/add-cursor-on-click)))

;; -------------------
;; diff-hl
;; -------------------
(use-package diff-hl
  :hook
  ((prog-mode . turn-on-diff-hl-mode)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh)))



;; TODO (change defun rewrite to advice)
(use-package quickrun
  :init
  (global-set-key [menu-bar tools quickrun] `(menu-item ,"Run Buffer" quickrun))  
  :config
  (setq quickrun-focus-p nil)
  ;; Move cursor out of the way when displaying output
  (advice-add 'quickrun--recenter
	      :after (lambda (&optional _)
		       (with-selected-window
			   (get-buffer-window quickrun--buffer-name)
			 (end-of-buffer))))
  :bind
  (([f8] . quickrun )))


;; Better Parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(show-paren-mode 1)

;; MAJOR MODES

(use-package markdown-mode
  :mode "\\.\\(m\\(ark\\)?down\\|md\\)$")

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode)
  :interpreter ("csharp" . csharp-mode))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package crystal-mode
  :mode "\\.cr\\'")

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package haml-mode
  :mode "\\.haml\\'")

;; Setup Splash Screen
(setq inhibit-startup-screen t)
(setq-default major-mode 'org-mode)
(setq-default initial-scratch-message ";; Emacs lisp scratch buffer. Happy hacking.\n\n")

;; Start with a blank buffer unless Emacs was started with a file to open.
;; Otherwise causes split window when opening file from command line or GUI.
(unless (< 1 (length command-line-args)) (setq initial-buffer-choice (lambda (&optional _)
			      (let ((buf (generate-new-buffer "untitled")))
				(set-buffer-major-mode buf)
				(message "New Buffer Started")
				(message (number-to-string (length command-line-args)))
				buf))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)

(require 'init-user nil t)
