;;; =======================
;;; + Misc defuns go here +
;;; =======================

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

;; Print the face found at the current point
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; XML pretty print
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region by written
  by Benjamin Ferrari. You need to have nxml-mode
  http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed
  to do this. The function inserts linebreaks to separate
  tags that have nothing but whitespace between them.
  It then indents the markup by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

;; JSON pretty print
(defun pretty-print-json-region ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
    )
  (message "Ah, much better!"))

;; Clearing eshell buffer
(defun eshell-clear ()
  "Clear the *eshell* buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (execute-kbd-macro [return])
    (message "erase eshell buffer")))
