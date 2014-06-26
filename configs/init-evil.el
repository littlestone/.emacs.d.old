;;; ====================================
;;; + An extensible vi layer for Emacs +
;;; ====================================

(require 'evil)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-numbers)
(require 'evil-visualstar)

(evil-mode 1)
(global-evil-leader-mode 1)
(global-evil-matchit-mode 1)

;; Clear insert state bindings.
(setcdr evil-insert-state-map nil)

;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)

;; Exit insert mode by pressing j and then j quickly
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

;; Set evil-shift-width to 2 space
(setq evil-shift-width 2)

;; Make sure escape gets back to normal state and quits things.
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-visual-state-map [escape] 'evil-normal-state)
(define-key evil-emacs-state-map [escape] 'evil-normal-state)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

;; Change cursor in different modes.
(setq evil-default-cursor 'box)
(setq evil-normal-state-cursor 'box)
(setq evil-visual-state-cursor 'hollow)
(setq evil-replace-state-cursor 'hbar)

;; Set the initial evil state that certain major modes will be in.
(evil-set-initial-state 'dired-mode 'normal)
(evil-set-initial-state 'ibuffer-mode 'normal)
(evil-set-initial-state 'fundamental-mode 'normal)
(evil-set-initial-state 'cider-repl-mode 'normal)
(evil-set-initial-state 'slime-repl-mode 'normal)
(evil-set-initial-state 'inferior-emacs-lisp-mode 'normal)
(evil-set-initial-state 'package-menu-mode 'emacs)
(evil-set-initial-state 'browse-kill-ring-mode 'emacs)
(evil-set-initial-state 'recentf-dialog-mode 'emacs)
(evil-set-initial-state 'magit-log-edit-mode 'emacs)
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'sldb-mode 'emacs)
(evil-set-initial-state 'org-mode 'emacs)
(evil-set-initial-state 'grep-mode 'emacs)
(evil-set-initial-state 'magit-mode 'emacs)
(evil-set-initial-state 'git-commit-mode 'emacs)
(evil-set-initial-state 'magit-status-mode 'emacs)

;; Change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#000000" . "#4169E1"))
                                 ((buffer-modified-p) '("#000000" . "#8470FF"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

;; Turn on/off highlight tail mode when entering/exiting evil insert state
(add-hook 'evil-insert-state-entry-hook 'highlight-tail-mode)
(add-hook 'evil-insert-state-exit-hook 'highlight-tail-mode)

;; Evil leader key bindings
(evil-leader/set-leader ",")
(evil-leader/set-key
  "+" 'evil-numbers/inc-at-pt
  "-" 'evil-numbers/dec-at-pt
  "a" 'ace-jump-char-mode
  "b" 'bury-buffer
  "e" 'ido-find-file
  "f" 'rgrep
  "g" 'google
  "h" 'helm-mini
  "i" 'ibuffer
  "j" 'dired-jump
  "k" 'kill-this-buffer
  "l" 'linum-mode
  "m" 'browse-url-at-point
  "o" 'other-window
  "q" 'read-only-mode
  "r" 'revert-this-buffer
  "sf" 'suspend-frame
  "t" 'my-toggle-fullscreen
  "u" 'undo-tree-visualize
  "w" 'save-buffer
  "x" 'ergoemacs-open-in-external-app
  "z" 'ergoemacs-compact-uncompact-block
  "TAB" 'eme-goto-scratch)

(provide 'init-evil)