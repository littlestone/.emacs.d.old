;;; prelude-global-keybindings.el --- Emacs Prelude: some useful keybindings.
;;
;; Copyright © 2011-2013 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Lots of useful keybindings.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      w32-apps-modifier 'hyper) ; Menu key

;; Package list
(global-set-key (kbd "C-c C-p") 'package-list-packages)

;; Smart M-x
(global-set-key (kbd "C-c M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; This is your old M-x.

;; Multiple curors
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-S-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c C-S-a") 'mc/edit-beginnings-of-lines)

;; Interactive macro expansion for Emacs Lisp
(define-key emacs-lisp-mode-map (kbd "C-c M-e") 'macrostep-expand)

;; Killing buffer
(global-set-key (kbd "C-c C-k") 'kill-this-buffer)

;; Reverting buffer
(global-set-key (kbd "C-c M-r") 'revert-this-buffer)

;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "C-c C-b") 'quick-switch-buffer)

;; Quickly switch to scratch buffer
(global-set-key (kbd "C-c <tab>") 'prelude-goto-scratch)

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'prelude-create-scratch-buffer)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; Comment/uncomment block
(global-set-key (kbd "C-c M-c") 'comment-or-uncomment-region)

;; Copy the whole lines
(global-set-key (kbd "C-c c") 'copy-whole-lines)

;; Copy file path to kill ring
(global-set-key (kbd "C-c M-w") 'copy-current-file-path)

;; Quick window navigation
(define-key global-map (kbd "C-x C-n") 'other-window)
(define-key global-map (kbd "C-x C-p") 'other-window-backward)

;; Window switching
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

;; Move buffer in multiple windows easily
(global-set-key (kbd "<M-up>") 'buf-move-up)
(global-set-key (kbd "<M-down>") 'buf-move-down)
(global-set-key (kbd "<M-left>") 'buf-move-left)
(global-set-key (kbd "<M-right>") 'buf-move-right)

;; Emulation of the vi % command
(global-set-key (kbd "%") 'goto-match-paren)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Indentation help
(global-set-key (kbd "C-^") 'prelude-top-join-line)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Start proced in a similar manner to dired
(unless (eq system-type 'darwin)
  (global-set-key (kbd "C-x p") 'proced))

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'smex)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)

;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; A complement to the zap-to-char command, that doesn't eat up the target character
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; Killing text
(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key (kbd "C-k") (bol-with-prefix kill-line))

;; Kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

;; Kill whole line easily
(global-set-key [remap kill-whole-line] 'prelude-kill-whole-line)

;; Show line number temporarily
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

;; Use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f11>") 'prelude-fullscreen))

;; Toggle menu-bar visibility
(global-set-key (kbd "<C-f10>") 'menu-bar-mode)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c j") 'ace-jump-mode)
(global-set-key (kbd "s-.") 'ace-jump-mode)
(global-set-key (kbd "C-c J") 'ace-jump-buffer)
(global-set-key (kbd "s->") 'ace-jump-buffer)
(global-set-key (kbd "s-w") 'ace-window)

;; Smarter compile
(global-set-key (kbd "<f5>") 'smarter-compile)

;; Open the current file or dired marked files in external app
(global-set-key (kbd "<f6>") 'ergoemacs-open-in-external-app)

;; Toggle linum-mode
(global-set-key (kbd "<f7>") 'linum-mode)

;; Toggle line wrap
(global-set-key (kbd "<f8>") 'toggle-truncate-lines)

;; Toggle window split
(global-set-key (kbd "<C-f8>") 'toggle-window-split)

;; Toggle whitespace-mode
(global-set-key (kbd "<f9>") 'whitespace-mode)

;; Clean up buffer
(global-set-key (kbd "<C-f9>") 'cleanup-buffer)

;; Toggle highlight defined or undefined symbols in Emacs-Lisp
(global-set-key (kbd "<C-f12>") 'hdefd-highlight-mode)

(provide 'prelude-global-keybindings)

;;; prelude-global-keybindings.el ends here
