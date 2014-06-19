;;; ===================================
;;; + Emacs incremental search tweaks +
;;; ===================================

;; Have emacs incremental search place the cursor at the beginning of the match
(add-hook 'isearch-mode-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(defun my-isearch-forward-to-beginning ()
  "Do a forward search and jump to the beginning of the search-term."
  (interactive)
  (isearch-repeat 'forward)
  (goto-char isearch-other-end))

(define-key isearch-mode-map (kbd "C-s") 'my-isearch-forward-to-beginning)

(provide 'init-isearch)
