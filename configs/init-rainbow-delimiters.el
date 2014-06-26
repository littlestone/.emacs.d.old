;;; ====================================================================
;;; + Highlights parens, brackets, and braces according to their depth +
;;; ====================================================================

(require 'rainbow-delimiters)

;; To enable it in all programming-related emacs modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;To enable it only in certain modes, add lines like the following:
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Using stronger colors
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))

;; Making unmatched parens stand out more
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error
                    :strike-through t)

(require 'paren) ; show-paren-mismatch is defined in paren.el
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'show-paren-mismatch)

;; Alternating dual color
(require 'cl-lib)

(defvar my-paren-dual-colors
  '("yellow" "green")
  )

(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (set-face-foreground
  (intern (format "rainbow-delimiters-depth-%d-face" index))
  (elt my-paren-dual-colors
       (if (cl-evenp index) 0 1))))

;; Bold outermost parens
(set-face-attribute 'rainbow-delimiters-depth-1-face nil
                    :weight 'bold)

(provide 'init-rainbow-delimiters)
