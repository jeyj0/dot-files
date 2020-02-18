;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jannis Jorre"
      user-mail-address "jannis@jorre.dev")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Monospace" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Easier moving through windows (Alt+[h,j,k,l])
(map! "M-h" 'evil-window-left)
(map! "M-j" 'evil-window-down)
(map! "M-k" 'evil-window-up)
(map! "M-l" 'evil-window-right)

;; switch to relative line numbering
(setq display-line-numbers-type 'relative)

;; open autocompletion more aggressively
(setq company-idle-delay 0.05
      company-minimum-prefix-length 1)

;; map SPC [stuff] to jupyter stuff
(map! :leader "j" 'ein:worksheet-goto-next-input)
(map! :leader "k" 'ein:worksheet-goto-prev-input)
(map! :leader "J" 'ein:worksheet-insert-cell-below)
(map! :leader "K" 'ein:worksheet-insert-cell-above)
(map! :leader "W" 'ein:notebook-save-notebook-command)

;; inline jupyter image results
(setq ein:output-area-inlined-images nil)

;; start emacs in fullscreen
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
