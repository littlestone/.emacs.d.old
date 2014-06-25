;;; ===================================================
;;; + This is where everything starts, live in Emacs! +
;;; ===================================================

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Run at all power
(setq disabled-command-function nil)

;; Run Emacs in server mode to speed up subsequent startups of Emacs significantly
(load "server")
(unless (server-running-p) (server-start))

;; Set up load path
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(add-to-list 'load-path (concat user-emacs-directory "defuns"))
(add-to-list 'load-path (concat user-emacs-directory "configs"))

;; Emacs's customization
(setq custom-file (concat user-emacs-directory "configs/custom.el"))
(load custom-file)

;; Common lisp library
(require 'cl)

;;;
;;;============================================================================
;;;

;; Emacs configurations
(require 'ipex)
(require 'appearance)
(require 'sane-defaults)
(require 'mode-mappings)
(require 'key-bindings)
(require 'packages)
(require 'init-erc)
(require 'init-w3m)
(require 'init-org)
(require 'init-evil)
(require 'init-dired)
(require 'init-slime)
(require 'init-cider)
(require 'init-magit)
(require 'init-isearch)
(require 'init-auto-complete)
(require 'init-ace-jump-mode)
(require 'init-rainbow-delimiters)

;; Emacs - Cygwin Customization
(if (eq system-type 'windows-nt)
  (require 'init-cygwin))

;; Third Party Functions (load all files in elisp-dir)
(setq elisp-dir (expand-file-name "elisp" user-emacs-directory))
(dolist (file (directory-files elisp-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; User Defined Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
