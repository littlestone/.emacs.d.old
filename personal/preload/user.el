;; All roads lead to $HOME
(setq default-directory "~/")

;; Disable scroll bar
(scroll-bar-mode -1)

;; Windows下Emacs中文字体设置
(if (string-equal system-type "gnu/linux")
    (set-default-font "Consolas-12")
  (set-default-font "Consolas-10"))
(set-fontset-font "fontset-default" 'gb18030 '("Microsoft YaHei" . "unicode-bmp")) ; 中文使用微软雅黑字体

;; Add all git related command path, exec-path is important for Magit, setenv is used by eshell
(if (eq system-type 'windows-nt)
    (progn
      (setenv "GIT_ASKPASS" "git-gui--askpass") ; fix magit push hung up issue on windows (require OpenSSH)
      (setq exec-path (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;" (getenv "PATH")))))

(provide 'user)
