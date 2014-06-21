;;; ==============================
;;; + Emacs Cygwin Customization +
;;; ==============================

;; Teach Emacs to recognize Cygwin's path names.
(setenv "PATH" (concat "c:/programs/cygwin/bin;" (getenv "PATH")))
(setq exec-path (cons "c:/programs/cygwin/bin/" exec-path))
(require 'cygwin-mount)
(cygwin-mount-activate)

;; Replace DOS shell with Cygwin Bash Shell
(add-hook 'comint-output-filter-functions
          'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt nil t)
(setq explicit-shell-file-name "bash.exe")
;; For subprocesses invoked via the shell
;; (e.g., "shell -c command")
(setq shell-file-name explicit-shell-file-name)

(provide 'init-cygwin)
