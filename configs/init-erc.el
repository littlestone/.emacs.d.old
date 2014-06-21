;;; ======================
;;; + Emacs IRC 基本设置 +
;;; ======================

(require 'erc)

;; 编码，尽量 utf-8 (#emacs-cn 也是用 utf-8):
(setq erc-default-coding-system '(utf-8 . utf-8))

;; 设置 nick, 全名: nick 就是登录时用的，full name 是别人查询你的时候显示的信息。(类似BBS 的 C-a )
(setq erc-nick "ghostsai"
      erc-user-full-name "Junjie Tang")

;; 登录后自动加入预定的 channels
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("#emacs"
         "#lisp"
         "#clojure")))

;; 如果你对某些消息或者某个人说的话特别感兴趣，我们可以通过关键字匹配对相关信息进行高亮
(erc-match-mode 1)
(setq erc-keywords '("lisp" "clojure"))
(setq erc-pals '("rms"))

;; 相反地，如果你对某些消息不感兴趣，比如有人进来啦，有人出去啦，如此这般一下就不会看到了
(setq erc-ignore-list nil)
(setq erc-hide-list
      '("JOIN" "PART" "QUIT" "MODE"))

;; 连接服务器或进入聊天室后自动执行预设操作
;; ERC 提供各种各样的 hook 让你在某个操作（登入 server, 进入channel等）之后执行一些你预设的操作。
;; 比如如果你有某个 channel 的管理员权限，可以在加入聊天室时自动转换到管理员身份：
;;(defun xwl-erc-auto-op ()
;;  (let ((b (buffer-name)))
;;    (when (string= b "#emacs-cn")
;;      (erc-message "PRIVMSG" (concat "chanserv op " b)))))
;;(add-hook 'erc-join-hook 'xwl-erc-auto-op)

(provide 'init-erc)
