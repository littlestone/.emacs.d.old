;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal notifier 
;; requires 'sudo gem install terminal-notifier'
;; stolen from erc-notifier

(defvar terminal-notifier-command (executable-find "terminal-notifier") "The path to terminal-notifier.")

; (terminal-notifier-notify "Emacs notification" "Something amusing happened")

(defun terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 terminal-notifier-command
                 "-title" title
                 "-message" message
                 "-activate" "org.gnu.Emacs"))

(defun timed-notification(time msg)
  (interactive "sNotification when (e.g: 2 minutes, 60 seconds, 3 days): \nsMessage: ")
  (run-at-time time nil (lambda (msg) (terminal-notifier-notify "Emacs" msg)) msg))
