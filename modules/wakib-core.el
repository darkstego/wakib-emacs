;; -*- lexical-binding: t -*-

;; Core behavior: wakib-keys, CUA selection, undo and better defaults
;; built from what already ships with Emacs.

;; -------------------
;; Wakib Keys
;; -------------------
(use-package wakib-keys
  :diminish wakib-keys
  :config
  (wakib-keys 1)
  (add-hook 'after-change-major-mode-hook 'wakib-update-major-mode-map)
  (add-hook 'menu-bar-update-hook 'wakib-update-minor-mode-maps)
  ;; Redo next to undo (C-z). undo-redo is the built-in linear redo;
  ;; vundo (below) gives access to the full undo tree.
  (define-key wakib-keys-overriding-map (kbd "C-S-z") 'undo-redo)
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
;; Undo Tree Visualizer
;; -------------------
(use-package vundo
  ;; Shown to the user as C-e u (wakib remaps the C-x prefix)
  :bind (("C-x u" . vundo)))

;; -------------------
;; Selection and Editing
;; -------------------
(cua-selection-mode 1)
(define-key cua-global-keymap (kbd "<C-return>") nil)

;; Automatically insert closing parenthesis/quote
(electric-pair-mode 1)

;; M-i/M-k browse history in eval-expression prompts
(define-key read-expression-map [remap previous-line] 'previous-line-or-history-element)
(define-key read-expression-map [remap next-line] 'next-line-or-history-element)

;; -------------------
;; Better Defaults
;; -------------------
(savehist-mode 1)                       ; persist minibuffer history
(save-place-mode 1)                     ; reopen files at last position
(global-auto-revert-mode 1)             ; keep buffers in sync with disk
(setq global-auto-revert-non-file-buffers t)

(recentf-mode 1)
(setq recentf-max-saved-items 100)

(when (display-graphic-p)
  (pixel-scroll-precision-mode 1))      ; smooth scrolling

(setq use-short-answers t)              ; y/n instead of typing "yes"
(setq ring-bell-function #'ignore)      ; no beeping

;; Don't recenter the window whenever point scrolls out of it; move
;; just enough to keep it visible, and keep the screen still when
;; paging up and down
(setq scroll-conservatively 20)
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)
(setq scroll-error-top-bottom t)

;; Deleting text in Emacs should not clobber what was copied in
;; another program: push it onto the kill ring first so C-S-v
;; (paste history) still finds it
(setq save-interprogram-paste-before-kill t)
(setq kill-do-not-save-duplicates t)

(setq delete-by-moving-to-trash t)      ; deleted files go to the trash
(setq vc-follow-symlinks t)             ; follow symlinks without asking
(setq uniquify-buffer-name-style 'forward) ; same-named buffers show their dir
(setq help-window-select t)             ; focus help windows when opened

;; Ediff: everything in one frame, diffs side by side
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; Disable backup and lockfile
;; I hate browsing directories and seeing 'Emacs was Here' everywhere
(setq create-lockfiles nil
      make-backup-files nil)

;; Auto-save (crash recovery) stays on, but collect the #file# saves
;; here instead of littering every directory with them
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save/" user-emacs-directory))
(unless (file-exists-p auto-save-list-file-prefix)
  (with-file-modes #o700
    (make-directory auto-save-list-file-prefix t)))
(setq auto-save-file-name-transforms
      `((".*" ,auto-save-list-file-prefix sha1)))
(setq kill-buffer-delete-auto-save-files t)

(setq frame-title-format '((:eval (buffer-name)) " [%+] Wakib Emacs"))

;; -------------------
;; Startup Buffer
;; -------------------
(setq-default initial-major-mode 'org-mode)
(setq-default initial-scratch-message "# Org-mode Disposable Scratch Pad for Temporary Notes.\n\n")

;; Start with a blank org-mode buffer unless Emacs was started with a
;; file to open (otherwise it causes a split window). Only this buffer
;; defaults to org: files with no recognizable mode stay fundamental.
(unless (< 1 (length command-line-args))
  (setq initial-buffer-choice
	(lambda (&optional _)
	  (let ((buf (generate-new-buffer "untitled")))
	    (with-current-buffer buf
	      (org-mode)
	      ;; Offer to save on exit, like C-n buffers
	      (setq buffer-offer-save t))
	    buf))))

(provide 'wakib-core)
