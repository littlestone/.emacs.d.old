;;; =========================
;;; + Buffer-related defuns +
;;; =========================

(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))
