;;;===================================
;;;+ Emacs appearance configurations +
;;;===================================

;; Initialize frame size
(setq initial-frame-alist '((top . 0) (left . 0) (width . 82) (height . 40)))

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Default color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'default-black t)

;; Set cursor color to white
(set-cursor-color "#ffffff")

;; Highlight the current line; set a custom face, so we can recognize from the normal marking (selection)
(global-hl-line-mode t)
(set-face-background 'hl-line "#303030")

;; Make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))

;; Windows下Emacs中文字体设置
(if (eq system-type 'gnu/linux)
  (set-default-font "Consolas-11.5")
  (set-default-font "Consolas-10"))
(set-fontset-font "fontset-default" 'gb18030 '("Microsoft YaHei" . "unicode-bmp")) ; 中文使用微软雅黑字体

(provide 'appearance)
