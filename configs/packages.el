;;; =====================================
;;; + Set up Emacs Lisp Package Archive +
;;; =====================================

;; Emacs package repository
(when (>= emacs-major-version 24)
  (require 'package)
  (require 'dash)
  (package-initialize)

  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
                           ("tromey" . "http://tromey.com/elpa/")))

  (when (not package-archive-contents)
    (package-refresh-contents))

  (defun packages-install (packages)
    (--each packages
      (when (not (package-installed-p it))
        (package-install it)))
    (delete-other-windows))

  ;; Install extensions if they're missing (Emacs native package manager)
  (defun init--install-packages ()
    (packages-install
     '(
       4clojure                            ; Open and evaluate 4clojure.com questions
       ac-nrepl                            ; auto-complete sources for Clojure using nrepl completions
       ac-slime                            ; An auto-complete source using slime completions
       ace-jump-mode                       ; a quick cursor location minor mode for emacs
       ascii                               ; ASCII code display.
       auto-complete                       ; Auto Completion for GNU Emacs
       autopair                            ; Automagically pair braces and quotes like TextMate
       browse-kill-ring                    ; interactively insert items from kill-ring
       buffer-move                         ; Swap buffers without typing C-x b on each window
       change-inner                        ; Change contents based on semantic units
       chm-view                            ; View CHM file.
       cider                               ; Clojure Intergrated Development Environment and REPL
       cl-lib                              ; Properly prefixed CL functions and macros
       cl-lib-highlight                    ; full cl-lib font-lock highlighting
       clojure-mode                        ; Major mode for Clojure code
       csharp-mode                         ; C# mode derived mode
       dash                                ; A modern list library for Emacs
       dash-functional                     ; Collection of useful combinators for Emacs List
       dired-single                        ; Reuse the current dired buffer to visit another directory
       epl                                 ; Emacs Package Library
       expand-region                       ; Increase selected region by semantic units
       fill-column-indicator               ; Graphically indicate the fill column
       fold-this                           ; Just fold this region please
       frame-cmds                          ; Frame and window commands (interactive functions).
       frame-fns                           ; Non-interactive frame and window functions.
       git-commit-mode                     ; Major mode for editing git commit messages
       git-rebase-mode                     ; Major mode for editing git rebase files
       goto-chg                            ; goto last change
       helm                                ; Helm is an Emacs incremental and narrowing framework
       ido-ubiquitous                      ; Use ido (nearly) everywhere
       indent-guide                        ; show vertical lines to guide indentation
       js2-mode                            ; Improved JavaScript editing mode
       json                                ; JavaScript Object Notation parser / generator
       json-mode                           ; Major mode for editing JSON files.
       json-reformat                       ; Reformatting tool for JSON
       json-snatcher                       ; Grabs the path to JSON values in a JSON file
       magit                               ; Control Git from Emacs
       markdown-mode                       ; Emacs Major mode for Markdown-formatted text files
       mouse-slider-mode                   ; scale numbers dragged under the mouse
       move-text                           ; Move current line or region with M-up or M-down.
       multiple-cursors                    ; Multiple cursors for Emacs
       paredit                             ; minor mode for editing parentheses
       pkg-info                            ; Information about packages
       popup                               ; Visual Popup User Interface
       rainbow-delimiters                  ; Highlight nested parens, brackets, braces a different color at each depth
       rainbow-mode                        ; Colorize color names in buffers
       s                                   ; The long lost Emacs string manipulation library
       simple-httpd                        ; pure elisp HTTP server
       skewer-mode                         ; Skewer support for live LESS stylesheet updates
       slime                               ; Superior Lisp Interaction Mode for Emacs
       smarter-compile                     ; an interface to `compile
       smex                                ; M-x interface with Ido-style fuzzing matching
       tabbar                              ; Display a tab bar in the header line
       undo-tree                           ; Treat undo history as a tree
       web-mode                            ; major mode for editing html templates
       zoom-frm                            ; Commands to zoom frame font size.
       )))

  (condition-case nil
      (init--install-packages)
    (error
     (package-refresh-contents)
     (init--install-packages))))

;;
;;=================================================================================
;;

;; Gimme some ido...everywhere
(ido-ubiquitous 1)

;; Treat undo history as a tree
(global-undo-tree-mode)

;; Display graphical indication of the fill column in a buffer
(add-hook 'prog-mode-hook 'turn-on-fci-mode)
(add-hook 'text-mode-hook 'turn-on-fci-mode)
(setq fci-rule-width 4)

;; Multiple cursor list file location
(setq mc/list-file (concat temporary-file-directory ".mc-lists.el"))

;; Enable autopair in all buffers
(require 'autopair)
(autopair-global-mode)

;; Display a tab bar in the header line
(require 'tabbar-tweak)
(tabbar-mode 1)

;; Show vertical lines to guide indentation
(require 'indent-guide)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

;; Draw a colourful "tail" while you write
(require 'highlight-tail)

;; An interface to `compile
(require 'smarter-compile)

;; GNU Go
(require 'gnugo-xpms)
(require 'gnugo)

(provide 'packages)
