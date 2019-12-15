;;; doom-dracula-alt-theme.el - based on https://draculatheme.com/
(require 'doom-themes)

;;
(defgroup doom-dracula-alt-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-dracula-alt-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-dracula-alt-theme
  :type 'boolean)

(defcustom doom-dracula-alt-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-dracula-alt-theme
  :type 'boolean)

(defcustom doom-dracula-alt-colorful-headers nil
  "If non-nil, headers in org-mode will be more colorful; which is truer to the
original Dracula Emacs theme."
  :group 'doom-dracula-alt-theme
  :type 'boolean)

(defcustom doom-dracula-alt-comment-bg doom-dracula-alt-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-dracula-alt-theme
  :type 'boolean)

(defcustom doom-dracula-alt-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-dracula-alt-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-dracula-alt
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#282a36" nil       nil            ))
   (bg-alt     '("#1E2029" nil       nil            ))
   (base0      '("#1E2029" "#1E2029"   "black"        ))
   (base1      '("#282a36" "#282a36" "brightblack"  ))
   (base2      '("#373844" "#373844" "brightblack"  ))
   (base3      '("#44475a" "#44475a" "brightblack"  ))
   (base4      '("#565761" "#565761" "brightblack"  ))
   (base5      '("#6272a4" "#6272a4" "brightblack"  ))
   (base6      '("#b6b6b2" "#b6b6b2" "brightblack"  ))
   (base7      '("#ccccc7" "#ccccc7" "brightblack"  ))
   (base8      '("#f8f8f2" "#f8f8f2" "white"        ))
   (fg         '("#f8f8f2" "#f8f8f2" "white"        ))
   (fg-alt     '("#e2e2dc" "#e2e2dc" "brightwhite"  ))

   (grey       base4)
   (red        '("#ff5555" "#ff6655" "red"          ))
   (orange     '("#ffb86c" "#ffb86c" "brightred"    ))
   (green      '("#50fa7b" "#50fa7b" "green"        ))
   (teal       '("#0189cc" "#0189cc" "brightgreen"  ))
   (yellow     '("#f1fa8c" "#f1fa8c" "yellow"       ))
   (blue       '("#61bfff" "#61bfff" "brightblue"   ))
   (dark-blue  '("#0189cc" "#0189cc" "blue"         ))
   (magenta    '("#ff79c6" "#ff79c6" "magenta"      ))
   (violet     '("#bd93f9" "#bd93f9" "brightmagenta"))
   (cyan       '("#8be9fd" "#8be9fd" "brightcyan"   ))
   (dark-cyan  '("#8be9fd" "#8be9fd" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      violet)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        orange)
   (comments       (if doom-dracula-alt-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-dracula-alt-brighter-comments dark-cyan base5) 0.25))
   (constants      cyan)
   (functions      green)
   (keywords       magenta)
   (methods        teal)
   (operators      violet)
   (type           blue)
   (strings        yellow)
   (variables      base8)
   (numbers        red)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (level1 magenta)
   (level2 violet)
   (level3 (if doom-dracula-alt-colorful-headers green   (doom-lighten violet 0.35)))
   (level4 (if doom-dracula-alt-colorful-headers yellow  (doom-lighten magenta 0.35)))
   (level5 (if doom-dracula-alt-colorful-headers cyan    (doom-lighten violet 0.6)))
   (level6 (if doom-dracula-alt-colorful-headers orange  (doom-lighten magenta 0.6)))
   (level7 (if doom-dracula-alt-colorful-headers blue    (doom-lighten violet 0.85)))
   (level8 (if doom-dracula-alt-colorful-headers magenta (doom-lighten magenta 0.85)))
   (level9 (if doom-dracula-alt-colorful-headers violet  (doom-lighten violet 0.95)))

   (hidden     base1)
   (-modeline-bright doom-dracula-alt-brighter-modeline)
   (-modeline-pad
    (when doom-dracula-alt-padded-modeline
      (if (integerp doom-dracula-alt-padded-modeline) doom-dracula-alt-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg

    (if -modeline-bright
        (doom-darken  magenta 0.675)
      (doom-darken bg 0.1))
    )
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken magenta 0.6)
      `(,(doom-darken (car bg) 0.075) ,@(cdr base1))
      ))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg) 0.075) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;; ((line-number &override) :foreground base4)
   ;; ((line-number-current-line &override) :foreground fg)
   ((line-number &override) :foreground base5 :distant-foreground nil)
   ((line-number-current-line &override) :foreground base7 :distant-foreground nil)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-dracula-alt-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (solaire-hl-line-face :background base2)
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-level-1 :background nil :foreground level1 :height 1.2 :weight 'bold)
   (org-level-2 :foreground level2 :weight 'bold)
   (org-level-3 :inherit 'org-level-2 :foreground level3)
   (org-level-4 :inherit 'org-level-2 :foreground level4)
   (org-level-5 :inherit 'org-level-2 :foreground level5)
   (org-level-6 :inherit 'org-level-2 :foreground level6)
   (org-level-7 :inherit 'org-level-2 :foreground level7)
   (org-todo :foreground orange :bold 'inherit :background (doom-darken base1 0.02))
   (org-done :foreground green :strike-through nil :background base2 :bold t)
   (org-headline-done :foreground base4 :strike-through nil)
   ((org-tag &override) :foreground (doom-lighten orange 0.3))
   (org-agenda-date :foreground cyan)
   (org-agenda-dimmed-todo-face :foreground comments)
   (org-agenda-done :foreground base4)
   (org-agenda-structure :foreground violet)
   (org-block            :background nil :foreground violet)
   (org-block-begin-line :background nil :foreground comments)
   (org-code :foreground yellow)
   (org-column :background base1)
   (org-column-title :background base1 :bold t :underline t)
   (org-date :foreground cyan)
   (org-document-info :foreground blue)
   (org-document-info-keyword :foreground comments)
   (org-ellipsis :foreground comments)
   (org-footnote :foreground blue)
   (org-headline-base :foreground comments :strike-through t :bold nil)
   (org-link :foreground orange :underline t :weight 'bold)
   (org-priority :foreground cyan)
   (org-scheduled :foreground green)
   (org-scheduled-previously :foreground yellow)
   (org-scheduled-today :foreground orange)
   (org-sexp-date :foreground base4)
   (org-special-keyword :foreground yellow)
   (org-table :foreground violet)
   (org-upcoming-deadline :foreground yellow)
   (org-warning :foreground magenta)
   )

  ;; --- extra variables ---------------------
  ;; ()
  )

(provide 'doom-dracula-alt-theme)
;;; doom-dracula-alt-theme.el ends here
