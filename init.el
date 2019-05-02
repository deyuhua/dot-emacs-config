;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;         main entry point of emacs config
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;;; Code:
(add-to-list 'load-path "~/.emacs.d/pkg")

(require 'pkg-environment)
(require 'pkg-packages)
(require 'pkg-appearance)
(require 'pkg-org-mode)
(require 'pkg-programming)
(require 'pkg-functions)
(require 'pkg-publish)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 	Start server for emacsclient
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(server-start)

(provide 'init)
;;; init.el ends here
