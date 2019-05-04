;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	coding theme settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Code:
(use-package doom-themes)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/pkg/themes"))
(require 'doom-dracula-alt)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; pick faces function
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun pkg-pick-gui-faces ()
  (custom-set-faces
   '(flycheck-error ((t (:box (:color "violet") :underline nil  :weight bold :foreground "violet") )))
   '(flycheck-warning ((t (:box (:color "yellow") :underline nil  :weight bold :foreground "yellow") )))
   '(flycheck-info ((t (:box (:color "green") :underline nil  :weight bold :foreground "green") )))
   )
  )

(defun pkg-pick-terminal-faces ()
  (custom-set-faces
   '(flycheck-error ((t (:background "violet" :weight bold :foreground "#808080") )))
   '(flycheck-warning ((t (:background "yellow" :weight bold :foreground "#808080") )))
   '(flycheck-info ((t (:background "green" :weight bold :foreground "#808080") )))
   '(vertical-border ((t (:background nil :foreground "grey"))))
   )
  )


;; ************************************************************
;; modify face base on env
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun pkg-modify-faces (frame)
  (select-frame frame)
  (progn
    (if (window-system frame)
        (pkg-pick-gui-faces)
      (pkg-pick-terminal-faces)
      )
    )
  )

(add-hook 'after-make-frame-functions 'pkg-modify-faces)

(if (display-graphic-p)
    (pkg-pick-gui-faces)
  (pkg-pick-terminal-faces)
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; modeline setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


;; FEATURE
;; (set-face-background hl-line-face "#00a8b5")

(provide 'pkg-appearance)
