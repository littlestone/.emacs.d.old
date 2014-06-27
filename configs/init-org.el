;;; ============
;;; + Org Mode +
;;; ============

(add-to-list 'load-path "~/.emacs.d/elisp/remember-2.0")
(require 'remember-autoloads)
(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n  %i\n  %a" "~/Dropbox/org/organizer.org")
        ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n  %a" "~/Dropbox/org/organizer.org")))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
  '(add-hook 'remember-mode-hook 'org-remember-apply-template))
(global-set-key (kbd "C-c r") 'remember)

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c l") 'org-store-link)
(setq org-agenda-include-diary t)
(setq org-log-done t)

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

(setq org-agenda-files (list "~/Dropbox/org/organizer.org"))

(provide 'init-org)