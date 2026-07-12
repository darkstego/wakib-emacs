;; -*- lexical-binding: t -*-

;; Development tools: git, project management, LSP, tree-sitter and
;; everything else that turns Emacs into an IDE.

;; Read subprocess output in large chunks; language servers stream a
;; lot of JSON and the small default chokes them
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; -------------------
;; Magit
;; -------------------
(use-package magit
  ;; Shown to the user as C-e g (wakib remaps the C-x prefix)
  :bind ("C-x g" . magit-status))

;; -------------------
;; Projects
;; -------------------
;; Built-in project.el; its commands live on C-e p
(setq project-switch-commands '((project-find-file "Find file")
				(project-find-regexp "Search")
				(project-dired "Dired")
				(project-eshell "Eshell")))

;; project.el ships no menu, so build one. Items are defined in
;; reverse order because define-key adds each entry to the top.
;; Keep the :keys strings in sync with project-prefix-map.
(with-eval-after-load 'wakib-keys
  (define-key wakib-keys-map [menu-bar project]
	      (list 'menu-item "Project" (make-sparse-keymap "Project")
		    :visible '(project-current)))
  (define-key wakib-keys-map [menu-bar project git]
	      '(menu-item "Git ..." magit-status :keys "C-e g"))
  (define-key wakib-keys-map [menu-bar project seperator1]
	      '(menu-item "--"))
  (define-key wakib-keys-map [menu-bar project kill-buffers]
	      '(menu-item "Close Project Buffers" project-kill-buffers :keys "C-e p k"))
  (define-key wakib-keys-map [menu-bar project eshell]
	      '(menu-item "Eshell" project-eshell :keys "C-e p e"))
  (define-key wakib-keys-map [menu-bar project seperator2]
	      '(menu-item "--"))
  (define-key wakib-keys-map [menu-bar project dired]
	      '(menu-item "Project Directory" project-dired :keys "C-e p D"))
  (define-key wakib-keys-map [menu-bar project search]
	      '(menu-item "Search in Project..." project-find-regexp :keys "C-e p g"))
  (define-key wakib-keys-map [menu-bar project switch]
	      '(menu-item "Switch Project..." project-switch-project :keys "C-e p p"))
  (define-key wakib-keys-map [menu-bar project find-file]
	      '(menu-item "Find File in Project..." project-find-file :keys "C-e p f")))

;; -------------------
;; Eglot (LSP)
;; -------------------
;; Built-in LSP client. Not enabled automatically: start it with
;; M-x eglot (or the Tools menu) in a buffer whose language server is
;; installed. Auto-start recipes live in refs/init-user.el.template
(use-package eglot
  :ensure nil
  :defer t
  :config
  (setq eglot-autoshutdown t)           ; stop the server with its last buffer
  (setq eglot-sync-connect 0)           ; never block the UI while connecting
  (setq eglot-report-progress nil)      ; no server spam in the minibuffer
  ;; Don't log every JSON-RPC message; it costs real memory and time
  (setq eglot-events-buffer-config '(:size 0 :format short))
  ;; Keep LSP features when jumping to definitions outside the project
  (setq eglot-extend-to-xref t))

(define-key global-map [menu-bar tools eglot]
	    '(menu-item "Start Language Server (Eglot)" eglot
			:help "IDE features for the current buffer's language"
			:enable (derived-mode-p 'prog-mode)))

;; Choose among multiple definitions in the minibuffer instead of a
;; separate window
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; -------------------
;; Tree-sitter
;; -------------------
;; Use the built-in tree-sitter major modes when their grammar is
;; available, and offer to install it (needs a C compiler) when not.
;; Falls back to the classic modes if the user declines.
(use-package treesit-auto
  ;; Skip entirely when Emacs was built without tree-sitter; the
  ;; classic modes below then handle everything on their own
  :if (fboundp 'treesit-parser-create)
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; -------------------
;; Classic Language Modes
;; -------------------
;; Entry points and fallbacks for popular languages whose tree-sitter
;; mode ships with Emacs but whose classic mode does not. treesit-auto
;; upgrades these to the ts-mode once its grammar is installed, and
;; declining the grammar leaves a fully working classic mode.
;; To support more languages, install their mode the same way (see
;; refs/init-user.el.template).
(use-package yaml-mode :defer t)
(use-package go-mode :defer t)
(use-package rust-mode :defer t)
(use-package lua-mode :defer t)
(use-package typescript-mode :defer t)
(use-package dockerfile-mode :defer t)
(use-package cmake-mode :defer t)

;; -------------------
;; diff-hl
;; -------------------
;; Highlight uncommitted changes in the fringe
(use-package diff-hl
  :hook ((prog-mode . turn-on-diff-hl-mode)
	 (magit-post-refresh . diff-hl-magit-post-refresh)))

;; -------------------
;; Quickrun
;; -------------------
(use-package quickrun
  :init
  (define-key global-map [menu-bar tools quickrun]
	      '(menu-item "Run Buffer" quickrun))
  :config
  (setq quickrun-focus-p nil)
  ;; Move cursor out of the way when displaying output
  (advice-add 'quickrun--recenter
	      :after (lambda (&optional _)
		       (with-selected-window
			   (get-buffer-window quickrun--buffer-name)
			 (goto-char (point-max)))))
  :bind ([f8] . quickrun))

;; -------------------
;; Misc
;; -------------------
;; Color nested parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Respect .editorconfig files (built-in)
(use-package editorconfig
  :ensure nil
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(provide 'wakib-dev)
