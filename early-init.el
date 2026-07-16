;; -*- lexical-binding: t -*-

;; Loaded before the GUI and the package system are initialized.
;; Keep this file limited to frame and startup settings; all real
;; configuration belongs in init.el and modules/.

;; Run "emacs --debug-init" to bring back the warnings this file and
;; init.el silence for everyday use

;; Defer garbage collection during startup, restore sane limits after
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 32 1024 1024))
	    (setq gc-cons-percentage 0.1)))

;; Skip the special file handlers (TRAMP, compressed files...) while
;; loading the hundreds of plain .el/.eln files startup consists of
(defvar wakib--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; Files given on the command line are opened before the restore
;; below runs and may need the handlers (e.g. TRAMP paths)
(advice-add 'command-line-1 :around
	    (lambda (fn args-left)
	      (let ((file-name-handler-alist (if args-left
						 wakib--file-name-handler-alist
					       file-name-handler-alist)))
		(funcall fn args-left))))
(add-hook 'emacs-startup-hook
	  (lambda ()
	    ;; Merge instead of overwrite to keep handlers added during startup
	    (setq file-name-handler-alist
		  (delete-dups (append file-name-handler-alist
				       wakib--file-name-handler-alist)))))

;; Frame settings applied before the first frame is created.
;; The menu bar stays on: Wakib menus display the correct shortcuts,
;; which is a core part of making Emacs discoverable for beginners.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; A roomier frame than the tiny 80x36-character legacy default. Sized
;; in characters so it scales with the font; if it doesn't suit your
;; monitor, see the Frame Size section in refs/init-user.el.template
(push '(width . 120) default-frame-alist)
(push '(height . 50) default-frame-alist)

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq load-prefer-newer t)              ; Don't load outdated byte code

;; Font compacting is expensive, especially with icon fonts on Windows
(setq inhibit-compacting-font-caches t)

;; Packages are native-compiled in the background after install; log
;; the (harmless) compiler warnings quietly instead of popping up a
;; buffer over what the user is doing
(defvar native-comp-async-report-warnings-errors)
(setq native-comp-async-report-warnings-errors (if init-file-debug t 'silent))

;; Only pop up the *Warnings* buffer for errors; plain warnings are
;; still logged there and in *Messages*, just not shoved in the
;; user's face
(defvar warning-minimum-level)
(setq warning-minimum-level (if init-file-debug :warning :error))

;; If a stray ~/.emacs file exists, Emacs loads it INSTEAD of this
;; directory's init.el and Wakib silently never starts; turn that
;; into an error that says what to do. init.el sets the flag.
(defvar wakib--init-loaded nil)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (when (and (not wakib--init-loaded)
		       (or (file-exists-p "~/.emacs")
			   (file-exists-p "~/.emacs.el")))
	      (error "Wakib Emacs did not start: Emacs loaded ~/.emacs instead of init.el.  Delete or rename ~/.emacs (and ~/.emacs.el), then restart Emacs")))
	  102)
