;;; =========================================
;;; + Emacs text browser w3m configurations +
;;; =========================================
(require 'w3m)

(setq w3m-use-favicon nil)
(setq w3m-command-arguments '("-cookie" "-F"))
(setq w3m-use-cookies t)
(setq w3m-add-referer t)
(setq w3m-follow-redirection 10)
(setq w3m-cookie-accept-bad-cookies t)
(setq w3m-home-page "https://www.google.ca")
(setq w3m-default-display-inline-images t)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(provide 'init-w3m)
