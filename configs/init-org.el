;;; ============
;;; + Org Mode +
;;; ============

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-agenda-include-diary t)
(setq org-log-done t)

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c a") 'org-agenda)
            (define-key org-mode-map (kbd "C-c b") 'org-switchb)
            (define-key org-mode-map (kbd "C-c l") 'org-store-link)))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("STARTED" :foreground "HotPink" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "yellow" :weight bold)
              ("HOLD" :foreground "orange" :weight bold)
              ("CANCELLED" :foreground "RoyalBlue" :weight bold))))

(setq org-agenda-files (quote ("~/Dropbox/org/todo.org"))
      org-default-notes-file "~/Dropbox/org/notes.org"
      org-agenda-ndays 7
      org-deadline-warning-days 14
      org-reverse-note-order t)

;;; ============================================================================

(add-to-list 'load-path "~/.emacs.d/elisp/remember-2.0")
(require 'remember-autoloads)
(setq org-remember-templates
      (quote ((116 "* TODO %?\n  %u" "~/todo.org" "Tasks")
              (110 "* %u %?" "~/notes.org" "Notes"))))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
  '(add-hook 'remember-mode-hook 'org-remember-apply-template))
(global-set-key (kbd "C-c r") 'remember)

(provide 'init-org)