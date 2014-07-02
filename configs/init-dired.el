;;; ==============================
;;; + Emacs dired configurations +
;;; ==============================

(require 'dired)
(require 'dired-x) ; default key bindings C-x C-j (M-x dired-jump)

;; Move files between split panes
(setq dired-dwim-target t)

;; Delete un-empty directories recursivly without warning
(setq dired-recursive-deletes 'always)

;; Recursive copy without prompt
(setq dired-recursive-copies 'always)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Extra Emacs dired functionality (single buffer, directory jump, hiding hidden files, etc...)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode -1)))
(put 'dired-find-alternate-file 'disabled nil)

;; To use the single-buffer feature most effectively
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(provide 'init-dired)
