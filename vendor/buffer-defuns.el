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

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

;; Quickly goto *scratch* buffer
(defun prelude-goto-scratch ()
  "this sends you to the scratch buffer"
  (interactive)
  (let ((prelude-scratch-buffer (get-buffer-create "*scratch*")))
    (switch-to-buffer prelude-scratch-buffer)
    (lisp-interaction-mode)))
