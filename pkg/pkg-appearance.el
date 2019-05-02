;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	coding theme settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Code:
(use-package doom-themes)
(require 'doom-dracula-alt)

(if window-system
    (progn
      (custom-set-faces
       '(flycheck-error ((t (:box (:color "violet") :underline nil  :weight bold :foreground "violet") )))
       '(flycheck-warning ((t (:box (:color "yellow") :underline nil  :weight bold :foreground "yellow") )))
       '(flycheck-info ((t (:box (:color "green") :underline nil  :weight bold :foreground "green") )))
       ))
  (progn
    (custom-set-faces
     '(flycheck-error ((t (:background "violet" :weight bold :foreground "#808080") )))
     '(flycheck-warning ((t (:background "yellow" :weight bold :foreground "#808080") )))
     '(flycheck-info ((t (:background "green" :weight bold :foreground "#808080") )))
     ))
  )

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; FEATURE
;; (set-face-background hl-line-face "#00a8b5")

(provide 'pkg-appearance)
