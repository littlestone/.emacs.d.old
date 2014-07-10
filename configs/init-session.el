;;; =======================================================================
;;; + Remember all opened files with splitted windows and their positions +
;;; =======================================================================

(require 'session)
(require 'revive)
(require 'windows)
(require 'recentf)

;; Restores various variables (e.g., input histories) from your last session
(add-hook 'after-init-hook 'session-initialize)

;; -- load the saved windows automatically on boot
(add-hook 'window-setup-hook 'resume-windows)

;; -- save place in file
(setq-default save-place t)

;; --  use this command to quit and save your setup
(define-key ctl-x-map "C" 'see-you-again)

;; -- set up window saving !! Place at end of .emacs file
(win:startup-with-window)

(provide 'init-session)