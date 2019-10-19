;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	coding theme settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Code:

(if (not (display-graphic-p))
    (progn
      (use-package dracula-theme)
      (load-theme 'dracula))
  (load-theme 'doom-dracula)
  )

(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border "grey")

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; FEATURE
(set-face-background hl-line-face "3c4245")

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(provide 'pkg-appearance)
