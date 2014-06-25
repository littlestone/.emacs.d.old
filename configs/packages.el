;;; =====================================
;;; + Set up Emacs Lisp Package Archive +
;;; =====================================

;; IPEX Proxy
(when t
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . "isa1ids.ipex.network:8080")
          ("https" . "isa1ids.ipex.network:8080")))
  (setq url-http-proxy-basic-auth-storage
        (list (list "isa1ids.ipex.network:8080"
                    (cons "Input your LDAP UID !"
                          (base64-encode-string "LOGIN:PASSWORD"))))))

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
       ac-nrepl                            ; auto-complete sources for Clojure using nrepl completions
       ac-slime                            ; An auto-complete source using slime completions
       ace-jump-mode                       ; a quick cursor location minor mode for emacs
       auto-complete                       ; Auto Completion for GNU Emacs
       browse-kill-ring                    ; interactively insert items from kill-ring
       buffer-move                         ; Swap buffers without typing C-x b on each window
       cider                               ; Clojure Intergrated Development Environment and REPL
       cl-lib                              ; Properly prefixed CL functions and macros
       cl-lib-highlight                    ; full cl-lib font-lock highlighting
       clojure-mode                        ; Major mode for Clojure code
       csharp-mode                         ; C# mode derived mode
       dash                                ; A modern list library for Emacs
       dash-functional                     ; Collection of useful combinators for Emacs List
       dired-single                        ; Reuse the current dired buffer to visit another directory
       epl                                 ; Emacs Package Library
       evil                                ; extensible vi layer
       evil-args                           ; Motions and text objects for delimited arguments in Evil.
       evil-exchange                       ; Exchange text more easily within Evil
       evil-indent-textobject              ; evil textobjects based on indentation
       evil-leader                         ; let there be <leader>
       evil-matchit                        ; Vim matchit ported into Emacs (requires EVIL)
       evil-nerd-commenter                 ; Commnet/uncomment lines efficiently. Like Nerd Commenter in Vim
       evil-numbers                        ; increment/decrement numbers like in vim
       evil-surround                       ; emulate surround.vim from Vim
       evil-visualstar                     ; Starts a * or # search from visual selection
       expand-region                       ; Increase selected region by semantic units
       fill-column-indicator               ; Graphically indicate the fill column
       fold-this                           ; Just fold this region please
       git-commit-mode                     ; Major mode for editing git commit messages
       git-rebase-mode                     ; Major mode for editing git rebase files
       helm                                ; Emacs incremental and narrowing framework
       ido-ubiquitous                      ; Use ido (nearly) everywhere
       iy-go-to-char                       ; Go to next CHAR which is similar to "f" and "t" in vim
       js2-mode                            ; Improved JavaScript editing mode
       json                                ; JavaScript Object Notation parser / generator
       json-mode                           ; Major mode for editing JSON files.
       json-reformat                       ; Reformatting tool for JSON
       key-chord                           ; map pairs of simultaneously pressed keys to commands
       magit                               ; Control Git from Emacs
       markdown-mode                       ; Emacs Major mode for Markdown-formatted text files
       move-text                           ; Move current line or region with M-up or M-down.
       multiple-cursors                    ; Multiple cursors for Emacs
       pkg-info                            ; Information about packages
       popup                               ; Visual Popup User Interface
       rainbow-delimiters                  ; Highlight nested parens, brackets, braces a different color at each depth
       rainbow-mode                        ; Colorize color names in buffers
       s                                   ; The long lost Emacs string manipulation library
       slime                               ; Superior Lisp Interaction Mode for Emacs
       smarter-compile                     ; an interface to `compile
       smex                                ; M-x interface with Ido-style fuzzing matching
       tabbar                              ; Display a tab bar in the header line
       undo-tree                           ; Treat undo history as a tree
       w3m                                 ; an Emacs interface to w3m
       web-mode                            ; major mode for editing html templates
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

;; Display a tab bar in the header line
(require 'tabbar-tweak)
(tabbar-mode 1)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

;; Draw a colourful "tail" while you write
(require 'highlight-tail)

;; An interface to `compile
(require 'smarter-compile)

(provide 'packages)
