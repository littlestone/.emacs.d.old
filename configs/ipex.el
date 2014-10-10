;;; ==================================
;;; + IPEX Working Environment Setup +
;;; ==================================

(load-file "~/.emacs.d/defuns/misc-defuns.el")

;; Proxy & FTP
(when (-contains? '("10.1.150.179" "10.1.150.128") (car (last(get-ip-addresses)))) ; LAN/Wireless
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . "isa1ids.ipex.network:8080")
          ("https" . "isa1ids.ipex.network:8080")))
		  
  (setq url-http-proxy-basic-auth-storage
        (list (list "isa1ids.ipex.network:8080"
                     (cons "Input your LDAP UID !"
                           (base64-encode-string "juntan:000999"))))) ; "LOGIN:PASSWORD" for prompt
  
  (setq ange-ftp-default-account "juntan"
      ange-ftp-default-password "010101"
      ange-ftp-default-user "juntan"
      ange-ftp-disable-netrc-security-check nil))

(provide 'ipex)
