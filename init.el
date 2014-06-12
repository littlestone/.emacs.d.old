;;; ===================================================
;;; + This is where everything starts, live in Emacs! +
;;; ===================================================

;;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; No splash screen
(setq inhibit-startup-message t)

;;; Run at all power
(setq disabled-command-function nil)

;;; Run Emacs in server mode to speed up subsequent startups of Emacs significantly
(load "server")
(unless (server-running-p) (server-start))

;;; Set up load path
(add-to-list 'load-path (concat user-emacs-directory "defuns"))
(add-to-list 'load-path (concat user-emacs-directory "configs"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

;;; Emacs's customization
(setq custom-file (concat user-emacs-directory "configs/init-custom-configs.el"))
(load custom-file)

;;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;;; Common lisp library
(require 'cl)

;;;
;;;============================================================================
;;;

;;; Emacs default initialization
(require 'init-appearance)
(require 'init-sane-defaults)
(require 'init-packages)
(require 'init-evil)
(require 'init-erc)
(require 'init-w3m)
(require 'init-dired)
(require 'init-slime)
(require 'init-cider)
(require 'init-magit)
(require 'init-isearch)
(require 'init-auto-complete)
(require 'init-ace-jump-mode)
(require 'init-rainbow-delimiters)
(require 'init-mode-mappings)
(require 'init-key-bindings)