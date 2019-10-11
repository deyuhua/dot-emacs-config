;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;         main entry point of emacs config
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;;; Code:
(add-to-list 'load-path "~/.emacs.d/core/+conf")
(add-to-list 'load-path "~/.emacs.d/core/+orgs")
(add-to-list 'load-path "~/.emacs.d/core/+private")
(add-to-list 'load-path "~/.emacs.d/core/+tools")
(add-to-list 'load-path "~/.emacs.d/core/+lang")

(require 'conf)
(require 'orgs)
(require 'lang)
(require 'private)
(require 'tools)
(require 'lang)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 	Start server for emacsclient
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(if (window-system)
    (server-start))

(provide 'init)
;;; init.el ends here
