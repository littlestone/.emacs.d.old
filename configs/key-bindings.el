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

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; This is your old M-x.

;; Expand region (increases selected region by semantic units)
(global-set-key (kbd "C-'") 'er/expand-region)

;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Providing a variety of completions and expansions
(global-set-key (kbd "M-/") 'hippie-expand)

;; Multiple curors
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-S-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c C-S-a") 'mc/edit-beginnings-of-lines)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "M-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-M-h") 'kill-region-or-backward-word)

;; Help should search more than just commands
(global-set-key (kbd "<f1> a") 'apropos)

;; Indentation help
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ; which used to be transpose-words
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Interactive selective display
(global-set-key (kbd "C-x $") 'inc-selective-display)

;; Change next underscore with a camel case
(global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)
(global-set-key (kbd "M-s M--") 'snakeify-current-word)

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (lambda () (interactive) (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (lambda () (interactive) (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (lambda () (interactive) (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (lambda () (interactive) (replace-region-by 's-upper-camel-case)))

;; Open above / below / in between
(global-set-key (kbd "C-c C-p") 'open-line-above)
(global-set-key (kbd "C-c C-n") 'open-line-below)
(global-set-key (kbd "C-c C-d") 'duplicate-current-line-or-region)

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)

;; Make eshell more convenient
(global-set-key (kbd "C-S-z") 'eshell)

;; Delete active frame
(global-set-key (kbd "C-x M-z") 'delete-frame)

;; Zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "s-z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))

(global-set-key (kbd "M-Z") (lambda (char) (interactive "cZap to char: ") (zap-to-char 1 char)))
(global-set-key (kbd "s-Z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))

;; Go to next CHAR which is similar to "f" and "t" in vim
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

;; Emulation of the vi % command
(global-set-key (kbd "%") 'goto-match-paren)

;; vim's ci and co commands
(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)

(global-set-key (kbd "s-i") 'copy-inner)
(global-set-key (kbd "s-o") 'copy-outer)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
(global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c M-r") 'revert-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "C-c C-b") 'quick-switch-buffer)

;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Copy file path to kill ring
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)

;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

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
(global-set-key [C-S-wheel-up] 'zoom-in)
(global-set-key [C-S-wheel-down] 'zoom-out)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; To test small elisp code changes easily with eval-region
(global-set-key (kbd "C-c C-r") 'eval-region)

;; Navigation bindings
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Completion at point
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Eval buffer
(global-set-key (kbd "C-c M-e") 'eval-buffer)

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

;; Mu4e
(global-set-key (kbd "C-x M") 'mu4e-up-to-date-status)

;; Clever newlines
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-S-O") 'open-line-above)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Line movement
(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;; Fold the active region
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; Yank and indent
(global-set-key (kbd "C-S-y") 'yank-unindented)

;; Yank's numeric prefix argument should repeat the yank
(global-set-key (kbd "C-y") (lambda (n)
                              (interactive "p")
                              (dotimes (i (abs n)) (yank))))

;; Toggle quotes
(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Sorting
(global-set-key (kbd "M-s-l") 'sort-lines)

;; Increase number at point (or other change based on prefix arg)
(global-set-key (kbd "C-+") 'change-number-at-point)
(global-set-key (kbd "C--") 'subtract-number-at-point)

;; Browse visualized undo tree
(global-set-key (kbd "C-x u") 'undo-tree-visualize)

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Buffer file functions
(global-set-key (kbd "C-x t") 'touch-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-M-k") 'delete-current-buffer-file)

;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; Find files by name and display results in dired
(global-set-key (kbd "M-s f") 'find-name-dired)

;; ASCII at point
(global-set-key (kbd "C-c C-a") 'ascii-display)

;; w3m text web browser
(global-set-key (kbd "C-c w") 'w3m-gohome)

;;;
;;;==========================================================================
;;;

;; Smarter compile
(global-set-key (kbd "<f5>") 'smarter-compile)

;; Open the current file or dired marked files in external app
(global-set-key (kbd "<C-f5>") 'ergoemacs-open-in-external-app)

;; Toggle hightlight-tail-mode
(global-set-key (kbd "<f8>") 'highlight-tail-mode)

;; Toggle whitespace-mode
(global-set-key (kbd "<f9>") 'whitespace-mode)

;; Clean up all useless whitespaces and indent the current buffer
(global-set-key (kbd "<C-f9>") 'cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Toggle full screen
(global-set-key (kbd "<f11>") 'my-toggle-fullscreen)

;; Toggle window split (vertical or horizontical)
(global-set-key (kbd "<C-f11>") 'toggle-window-split)

(provide 'key-bindings)
