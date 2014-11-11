;; All roads lead to $HOME
(setq default-directory "~/")

;; Disable scroll bar in each window
(scroll-bar-mode -1)

;; Windows下Emacs中文字体设置
(if (string-equal system-type "gnu/linux")
    (set-default-font "Consolas-12")
  (set-default-font "Consolas-10"))
(set-fontset-font "fontset-default" 'gb18030 '("Microsoft YaHei" . "unicode-bmp")) ; 中文使用微软雅黑字体

(provide 'user)
