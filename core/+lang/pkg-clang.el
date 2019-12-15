;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	C/C++ IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)

(use-package cc-mode
  :bind (:map c-mode-map
	      ("M-h" . helm-gtags-pop-stack)
	      ("M-l" . helm-gtags-find-tag))
  :config
  (progn
    (require 'compile)
    (c-toggle-auto-newline 1)))

(use-package disaster
  :commands (disaster))

(use-package clang-format)

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))

(use-package company-c-headers)

(use-package gdb-mi
  :init
  (setq
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))


(provide 'pkg-clang)
