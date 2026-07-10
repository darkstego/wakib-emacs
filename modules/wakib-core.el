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

;; Disable backup and lockfile
;; I hate browsing directories and seeing 'Emacs was Here' everywhere
(setq create-lockfiles nil
      make-backup-files nil)

(setq frame-title-format '((:eval (buffer-name)) " [%+] Wakib Emacs"))

;; -------------------
;; Startup Buffer
;; -------------------
(setq-default major-mode 'org-mode)
(setq-default initial-scratch-message ";; Emacs lisp scratch buffer. Happy hacking.\n\n")

;; Start with a blank buffer unless Emacs was started with a file to open.
;; Otherwise causes split window when opening file from command line or GUI.
(unless (< 1 (length command-line-args))
  (setq initial-buffer-choice
	(lambda (&optional _)
	  (let ((buf (generate-new-buffer "untitled")))
	    (set-buffer-major-mode buf)
	    buf))))

(provide 'wakib-core)
