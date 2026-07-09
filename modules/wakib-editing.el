;; -*- lexical-binding: t -*-

;; Text editing tools: quick navigation, multiple cursors, snippets,
;; spell checking and text major modes.

;; -------------------
;; avy
;; -------------------
;; Jump to any visible character
(use-package avy
  :bind ("M-m" . avy-goto-char))

;; -------------------
;; expand-region
;; -------------------
(use-package expand-region
  :bind ("M-A" . er/expand-region))

;; -------------------
;; multiple-cursors
;; -------------------
;; TODO - Advice CUA-keyboard-quit to quit mc and rrm
(use-package multiple-cursors
  :init
  (setq mc/always-run-for-all t)
  ;; Bound here (not :bind :map) so the keys work before the package loads
  (define-key wakib-keys-overriding-map (kbd "C-.") 'mc/mark-next-like-this)
  (define-key wakib-keys-overriding-map (kbd "C-,") 'mc/mark-previous-like-this)
  (define-key wakib-keys-overriding-map (kbd "<C-down-mouse-1>") 'mc/add-cursor-on-click)
  :config
  (define-key mc/keymap [remap keyboard-quit] 'mc/keyboard-quit)
  (define-key rectangular-region-mode-map [remap keyboard-quit] 'rrm/keyboard-quit)
  :bind ("M-S" . set-rectangular-region-anchor))

;; -------------------
;; Yasnippet
;; -------------------
(use-package yasnippet-snippets
  :defer t)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :diminish yas-minor-mode
  :config
  (require 'yasnippet-snippets)
  (yas-reload-all)
  ;; M-;/M-: jump between snippet fields
  (define-key yas-keymap [remap wakib-next] 'yas-next-field)
  (define-key yas-keymap [remap wakib-previous] 'yas-prev-field))

(use-package consult-yasnippet
  :bind ("C-y" . consult-yasnippet))

;; -------------------
;; Spell Checking
;; -------------------
;; Corrections use the minibuffer (vertico). Enable spell checking
;; with M-x flyspell-mode (or flyspell-prog-mode for code comments).
(use-package flyspell-correct
  :defer t)

;; M-; on a misspelled word corrects it. wakib-next-more is the
;; extension hook wakib-keys provides for exactly this (see its
;; docstring); advice keeps the override intact across reloads.
(defun wakib--flyspell-next-more (&optional _arg)
  "Correct the word at point when it is misspelled."
  (interactive "p")
  (when (and (bound-and-true-p flyspell-mode)
	     (or (wakib-find-overlays-specifying 'flyspell-overlay)
		 (save-excursion
		   (backward-word)
		   (wakib-find-overlays-specifying 'flyspell-overlay))))
    (flyspell-correct-wrapper)))
(advice-add 'wakib-next-more :override #'wakib--flyspell-next-more)

(with-eval-after-load 'flyspell
  (define-key flyspell-mouse-map [mouse-2] nil)
  (define-key flyspell-mouse-map [mouse-3] 'flyspell-correct-word))

;; -------------------
;; Org
;; -------------------
(setq org-export-with-toc nil)
(setq org-support-shift-select t)

;; -------------------
;; Markdown
;; -------------------
(use-package markdown-mode
  :mode "\\.\\(m\\(ark\\)?down\\|md\\)$")

(provide 'wakib-editing)
