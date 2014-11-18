;;; user.el --- Prelude's personal configuration entry point.

;; Run Emacs in server mode to speed up subsequent startups of Emacs significantl
(load "server")
(unless (server-running-p) (server-start))

;; All roads lead to $HOME
(setq default-directory "~/")

;; Show keystrokes in progress.
(setq echo-keystrokes 0.1)

;; When deleted a file goes to the OS's trash folder
(setq delete-by-moving-to-trash t)

;; UTF-8 please
(setq locale-coding-system 'utf-8-unix) ; pretty
(set-terminal-coding-system 'utf-8-unix) ; pretty
(set-keyboard-coding-system 'utf-8-unix) ; pretty
(set-buffer-file-coding-system 'utf-8-unix) ; please
(prefer-coding-system 'utf-8-unix) ; pretty

;; CUA rectangle support
(setq cua-enable-cua-keys nil)
(setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
(cua-mode)

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

;; Use GNU W32 Utils find and grep for Windows
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  (setenv "PATH" (concat "C:\\GNU\\bin\\gnuwin32\\bin;" (getenv "PATH")))
  (setq find-program "C:\\GNU\\bin\\gnuwin32\\bin\\find.exe"
        grep-program "C:\\GNU\\bin\\gnuwin32\\bin\\grep.exe"))

(provide 'user)
