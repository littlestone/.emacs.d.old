;;; ====================================
;;; + Emacs keybindings configurations +
;;; ====================================

;; Make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      w32-apps-modifier 'hyper) ; Menu key

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; This is your old M-x.

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Make shell more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'eshell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Quick look for recent opened files
(global-set-key (kbd "C-x f") 'recentf-open-files)

;; New rectangle mark mode in Emacs 24.4
(global-set-key (kbd "C-@") 'rectangle-mark-mode)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "C-c C-o") 'quick-switch-buffer)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; To test small elisp code changes easily with eval-region
(global-set-key (kbd "C-c C-r") 'eval-region)

;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Providing a variety of completions and expansions
(global-set-key (kbd "M-/") 'hippie-expand)

;; Browse the kill ring
(global-set-key (kbd "C-x u") 'browse-kill-ring)

;; Multiple cursors
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-S-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c C-S-a") 'mc/edit-beginnings-of-lines)

;; Go to next CHAR which is similar to "f" and "t" in vim
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Open above / below / in between
(global-set-key (kbd "C-c C-p") 'open-line-above)
(global-set-key (kbd "C-c C-n") 'open-line-below)
(global-set-key (kbd "C-c C-d") 'duplicate-current-line-or-region)

;; Emulation of the vi % command
(global-set-key (kbd "%") 'goto-match-paren)

;;;
;;;==========================================================================
;;;

;; Google search
(global-set-key (kbd "<f1>") 'google)

;; Smarter compile
(global-set-key (kbd "<f5>") 'smarter-compile)

;; Open the current file or dired marked files in external app
(global-set-key (kbd "<C-f5>") 'ergoemacs-open-in-external-app)

;; Toggle whitespace-mode
(global-set-key (kbd "<f9>") 'whitespace-mode)

;; Clean up all useless whitespaces and indent the current buffer
(global-set-key (kbd "<C-f9>") 'cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Toggle highlight-tail-mode
(global-set-key (kbd "<f11>") 'highlight-tail-mode)

;; Toggle full screen
(global-set-key (kbd "<f12>") 'my-toggle-fullscreen)

;; Toggle window split (vertical or horizontical)
(global-set-key (kbd "<C-f12>") 'toggle-window-split)

;; Window switching
(windmove-default-keybindings) ;; Shift+direction

;; Move buffer in multiple windows easily
(global-set-key (kbd "<M-up>") 'buf-move-up)
(global-set-key (kbd "<M-down>") 'buf-move-down)
(global-set-key (kbd "<M-left>") 'buf-move-left)
(global-set-key (kbd "<M-right>") 'buf-move-right)

;; Resize window easily
(global-set-key (kbd "<M-S-left>") 'enlarge-window-horizontally)
(global-set-key (kbd "<M-S-right>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-S-up>") 'enlarge-window)
(global-set-key (kbd "<M-S-down>") 'shrink-window)

;; Zoom frame font size
(global-set-key [C-S-wheel-up]    'zoom-in)
(global-set-key [C-S-wheel-down]  'zoom-out)

(provide 'key-bindings)
