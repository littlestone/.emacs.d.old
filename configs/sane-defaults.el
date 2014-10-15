;;; ===================================================================
;;; + Better defaults section : fixing weird quirks and poor defaults +
;;; ===================================================================

;; All roads lead to $HOME
(setq default-directory "~/")

;; Write temporary files to own directory
(progn
  (defvar --temporary-directory (concat user-emacs-directory "temps"))
  (if (not (file-exists-p --temporary-directory))
      (make-directory --temporary-directory))

  ;; Local session.
  (unless (daemonp)
    (custom-set-variables '(session-save-file (expand-file-name "session" temporary-file-directory))))

  (setq temporary-file-directory (concat user-emacs-directory "temps/")
        save-place-file (expand-file-name "places" temporary-file-directory)
        savehist-file (expand-file-name "history" temporary-file-directory)
        recentf-save-file (expand-file-name "recentf" temporary-file-directory)
        abbrev-file-name (expand-file-name "abbrev_defs" temporary-file-directory)
        tramp-persistency-file-name (expand-file-name "tramp" temporary-file-directory)
        auto-save-list-file-prefix "~/.emacs.d/temps/auto-save-list/.saves-"
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; stop emacs's backup changing the file's creation date of the original file
(setq backup-by-copying t)

;; Save place in files between sessions
(setq-default save-place t)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode +1)
(setq recentf-max-saved-items 50) ; just 20 is too recent

;; Buffer full file name as title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8-unix) ; pretty
(set-terminal-coding-system 'utf-8-unix) ; pretty
(set-keyboard-coding-system 'utf-8-unix) ; pretty
(set-selection-coding-system 'utf-8-unix) ; please
(set-buffer-file-coding-system 'utf-8-unix) ; please
(prefer-coding-system 'utf-8-unix) ; pretty

;; Interactively do things
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Add parts of each file's directory to the buffer name if not unique
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; Let incremental search place the cursor at the beginning of the match
(defadvice isearch-exit (after move-to-match-beginning activate)
  (when (and isearch-forward isearch-success)
    (goto-char (match-beginning 0))))

;; Prevent issues with the Windows null device (NUL) when using cygwin find with rgrep.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
    ad-do-it))
(ad-activate 'grep-compute-defaults)

;; Auto-indent new lines
(add-hook 'prog-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))

;; Show active region
(transient-mark-mode +1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; CUA rectangle support
(setq cua-enable-cua-keys nil)
(setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
(cua-mode)

;; Enable the echo area to display information about a function or variable in the text where point is.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Enable pretty symbols for all programming modes
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

;; Sane defaults
(setq delete-by-moving-to-trash t             ; move deleted file to Recycle Bin
      save-interprogram-paste-before-kill t   ; preserve clipboard content in Emacs on Windows.
      apropos-do-all t                        ; apropos commands perfom more extensive searches than default.
      mouse-yank-at-point t                   ; 不要在鼠标点击的那个地方插入剪贴板内容。我不喜欢那样，经常把我的文档搞的一团糟。我觉得先用光标定位，然后鼠标中键点击要好的多。不管你的光标在文档的那个位置，或是在 minibuffer，鼠标中键一点击，X selection 的内容就被插入到那个位置。
      mode-require-final-newline nil          ; 文件末尾不要自动插入空行
      default-major-mode 'text-mode           ; 把缺省的major mode设为text-mode
      kill-ring-max 200                       ; 用一个很大的kill ring，防止不小心删掉重要的东西
      echo-keystrokes 0.1                     ; show keystrokes in progress.
      read-quoted-char-radix 10               ; use of decimal sequences instead of octal to insert a non-graphic character (16 for hexadecimal)
      gc-cons-threshold 20000000              ; don't be so stingy on the memory, we have lots now. It's the distant future.
      global-auto-revert-non-file-buffers t   ; auto refresh dired,
      dired-dwim-target t                     ; copy from one dired dir to the next dired dir shown in a split window
      global-auto-revert-mode t               ; auto refresh buffers
      auto-revert-verbose nil                 ; but be quiet about it.
      shift-select-mode nil                   ; real emacs knights don't use shift to mark things.
      eval-expression-print-length nil        ; do not truncate messages in the echo area
      desktop-save-mode 1                     ; save/restore opened files and windows config, 0 for off
      load-prefer-newer t                     ; never accidentally using outdated compiled files
      )

;; 设置有用的个人信息，这在很多地方有用
(setq user-full-name "Sai")
(setq user-mail-address "razorsniper@gmail.com")

;; 不用 TAB 字符来indent, 这会引起很多奇怪的错误。编辑 Makefile 的时候也不用担心，因为 makefile-mode 会把 TAB 键设置成真正的 TAB 字符，并且加亮显示的
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 120 4))
(setq indent-line-function 'insert-tab)
(setq c-default-style "linux")
(setq c-basic-offset 4)
(c-set-offset 'comment-intro 0)

;; 设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插入两个空格
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; 防止页面滚动时跳动，scroll-margin 3 可以在靠近屏幕边沿3行时就开始滚动，可以很好的看到上下文
(setq scroll-margin 3
      scroll-conservatively 10000)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region craft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; My preferred setting for the 'hippie-expand functions
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-visible
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Auto-indent yanked (pasted) code
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(provide 'sane-defaults)
