;; -*- lexical-binding: t -*-

;; Completion everywhere: the minibuffer (vertico + orderless +
;; marginalia + consult + embark) and inside buffers (corfu + cape).
;; All of these build on the standard Emacs completion APIs, so any
;; command that uses completing-read benefits automatically.

;; -------------------
;; Vertico
;; -------------------
(use-package vertico
  :config
  (vertico-mode 1)
  ;; M-i/M-k already move through candidates (vertico remaps
  ;; previous-line/next-line); M-;/M-: should do the same
  (define-key vertico-map [remap wakib-next] 'vertico-next)
  (define-key vertico-map [remap wakib-previous] 'vertico-previous))

;; -------------------
;; Orderless
;; -------------------
;; Type space-separated terms in any order to filter candidates
(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

;; -------------------
;; Marginalia
;; -------------------
;; Annotate candidates (docstrings, file sizes, keybindings...)
(use-package marginalia
  :config
  (marginalia-mode 1))

;; -------------------
;; Consult
;; -------------------
;; Remaps keep the menu bar and wakib key hints correct
(use-package consult
  :bind (([remap switch-to-buffer] . consult-buffer) ; C-b
	 ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
	 ([remap goto-line] . consult-goto-line)     ; M-M
	 ([remap imenu] . consult-imenu)
	 ([remap yank-pop] . consult-yank-pop))
  :init
  (define-key wakib-keys-overriding-map (kbd "C-S-v") 'consult-yank-pop)
  ;; Use ripgrep for project search (C-e p g) when available
  (when (executable-find "rg")
    (define-key global-map [remap project-find-regexp] 'consult-ripgrep))
  :config
  (setq consult-narrow-key "<"))

;; -------------------
;; Embark
;; -------------------
;; Act on the thing at point or the current minibuffer candidate
(use-package embark
  :bind ("M-'" . embark-act))

(use-package embark-consult
  :after (embark consult))

;; -------------------
;; Corfu
;; -------------------
;; In-buffer completion popup (replaces company)
(use-package corfu
  :config
  (setq corfu-auto t
	corfu-auto-prefix 2
	corfu-cycle t)
  (global-corfu-mode 1)
  ;; M-i/M-k work out of the box (corfu remaps previous/next-line);
  ;; make M-;/M-: and ESC behave the same as elsewhere in wakib
  (define-key corfu-map [remap wakib-next] 'corfu-next)
  (define-key corfu-map [remap wakib-previous] 'corfu-previous)
  (define-key corfu-map [remap wakib-keyboard-quit] 'corfu-quit)
  ;; Tab completes (press again to insert candidate), Return always
  ;; inserts a newline so the popup never blocks typing
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "<return>") nil)
  (define-key corfu-map (kbd "TAB") 'corfu-complete)
  (define-key corfu-map (kbd "<tab>") 'corfu-complete))

;; Corfu popups need child frames; this provides a fallback in the terminal
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; -------------------
;; Cape
;; -------------------
;; Extra completion sources for corfu
(use-package cape
  :defer t
  :init
  ;; cape functions are autoloaded, so adding them here doesn't load cape
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(provide 'wakib-completion)
