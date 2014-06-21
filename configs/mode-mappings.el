;;; ===========================================
;;; + Emacs various major mode configurations +
;;; ===========================================

;; Emacs lisp
(autoload 'emacs-lisp-mode "emacs-lisp-mode" "Emacs Lisp Mode" t)
(add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))

;; Common lisp
(autoload 'lisp-mode "lisp-mode" "Lisp Mode" t)
(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))

;; Clojure
(autoload 'clojure-mode "clojure-mode" "Clojure Mode" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; Org-mode
(autoload 'org-mode "org-mode" "Org Mode" t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode" "Markdown Mode" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; JavaScript
(autoload 'js2-mode "js2-mode" "js2 Mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

;; C# mode
(autoload 'csharp-mode "csharp-mode" "Csharp Mode" t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; Unibasic mode
(autoload 'unibasic-mode "unibasic-mode" "Unibasic Mode" t)
(add-to-list 'auto-mode-alist '("\\.ub$" . unibasic-mode))

;; MQL mode
(autoload 'mql-mode "mql-mode" "MQL Mode" t)
(add-to-list 'auto-mode-alist '("\\.mq4$" . mql-mode))
(add-to-list 'auto-mode-alist '("\\.mq5$" . mql-mode))

;; Visual Basic mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic Mode" t)
(add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode)) ; VBscript
(add-to-list 'auto-mode-alist '("\\.vb\\'" . visual-basic-mode))  ; visual basic .NET file
(add-to-list 'auto-mode-alist '("\\.bas\\'" . visual-basic-mode)) ; visual basic form
(add-to-list 'auto-mode-alist '("\\.frm\\'" . visual-basic-mode)) ; basic language source
(add-to-list 'auto-mode-alist '("\\.cls\\'" . visual-basic-mode)) ; C++ class definition file

;; Dos script mode
(autoload 'visual-basic-mode "dos-mode" "Dos Mode" t)
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode)) ; dos script

(provide 'mode-mappings)
