;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Useful tools setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Code:
;; yaml file
(use-package yaml-mode)

;; bison and flex file
(use-package bison-mode)

;; markdown preview tool
(custom-set-variables
 '(livedown-autostart nil) ; automatically open preview when opening markdown files
 '(livedown-open t)        ; automatically open the browser window
 '(livedown-port 1337)     ; port for livedown server
 '(livedown-browser nil))  ; browser to use

(use-package markdown-toc)

(provide 'pkg-tools)
