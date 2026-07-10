;; -*- lexical-binding: t -*-

;; Look and feel: theme, menu bar, key discovery and window management.

;; -------------------
;; Theme
;; -------------------
;; Built-in modus theme. To use another theme, load it from
;; user/init-user.el; it will win because it loads last.
(load-theme 'modus-vivendi-tinted t)

;; -------------------
;; Menu Bar
;; -------------------
;; Menus are kept on (see early-init.el) except in the terminal.
(unless (display-graphic-p)
  (menu-bar-mode -1))

(define-key global-map [menu-bar file new-file]
	    `(menu-item "New File..." wakib-new-empty-buffer
			:enable (menu-bar-non-minibuffer-window-p)
			:help "Create a new blank buffer"
			:key-sequence ,(kbd "C-n")))

(define-key global-map [menu-bar file open-file]
	    `(menu-item "Open File..." find-file
			:enable (menu-bar-non-minibuffer-window-p)
			:help "Read an existing or new file from disk"
			:key-sequence ,(kbd "C-o")))

(define-key global-map [menu-bar file dired]
	    '(menu-item "Open Directory..." dired
			:enable (menu-bar-non-minibuffer-window-p)
			:help "Browse a directory, to operate on its files"
			:keys "C-e d"))

(define-key global-map [menu-bar file insert-file]
	    '(menu-item "Insert File..." insert-file
			:enable (menu-bar-non-minibuffer-window-p)
			:help "Insert another file into current buffer"
			:keys "C-e i"))

(global-unset-key [menu-bar options cua-mode])

;; -------------------
;; which-key
;; -------------------
;; Built into Emacs 30. Shows available keys after a prefix (e.g. C-e)
(use-package which-key
  :ensure nil
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (which-key-mode))

;; -------------------
;; Window Switching
;; -------------------
(use-package ace-window
  :bind ("M-H" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame))

(provide 'wakib-ui)
