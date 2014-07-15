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

;; C++11
(add-hook
 'c++-mode-hook
 '(lambda()
    ;; We could place some regexes into `c-mode-common-hook', but note that their evaluation order
    ;; matters.
    (font-lock-add-keywords
     nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; namespace names and tags - these are rendered as constants by cc-mode
           ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
           ;;  new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
           ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
           ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
           ;; integer/float/scientific numbers
           ("\\<[-+]?[0-9]*\\.?[0-9]+\\([uUlL]+\\|[eE][-+]?[0-9]+\\)?[fFlL]?\\>" . font-lock-constant-face)
           ;; c++11 string literals
           ;;       L"wide string"
           ;;       L"wide string with UNICODE codepoint: \u2018"
           ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
           ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
           ;;       R"(user-defined literal)"
           ;;       R"( a "quot'd" string )"
           ;;       R"delimiter(The String Data" )delimiter"
           ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
           ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
           (   "\\<\\([uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?)[^\\s-\\\\()]\\{0,16\\}\"\\)\\>" 1 font-lock-string-face t)  ; actual string
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

           ;; user-defined types (rather project-specific)
           ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
           ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
    ) t)

(provide 'mode-mappings)
