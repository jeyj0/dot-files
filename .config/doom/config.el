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
(setq doom-font (font-spec :family "Hack" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


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
(map! :leader "h" 'evil-window-left
      :leader "j" 'evil-window-down
      :leader "k" 'evil-window-up
      :leader "l" 'evil-window-right)

;; Easier moving through buffers (Alt+[U,I])
(map! :leader "u" 'evil-prev-buffer
      :leader "i" 'evil-next-buffer
      :leader "bq" 'doom/save-and-kill-buffer)

;; Easier switching tabs (Alt+[u,i])
(map! :leader "U" '+workspace:switch-previous
      :leader "I" '+workspace:switch-next)

;; open autocompletion more aggressively
(setq company-idle-delay 0.05
      company-minimum-prefix-length 1)

;; map SPC [stuff] to jupyter stuff
;; (map! :leader "j" 'ein:worksheet-goto-next-input)
;; (map! :leader "k" 'ein:worksheet-goto-prev-input)
;; (map! :leader "J" 'ein:worksheet-insert-cell-below)
;; (map! :leader "K" 'ein:worksheet-insert-cell-above)
;; (map! :leader "W" 'ein:notebook-save-notebook-command)

;; inline jupyter image results
;; (setq ein:output-area-inlined-images nil)

;; start emacs in fullscreen
(add-hook 'window-setup-hook #'toggle-frame-maximized)

;; tell projectile where projects are to be found
(setq projectile-project-search-path '("~/projects/"))

;; load host-specific settings
;;;; get hostname
(defvar host (substring (shell-command-to-string "hostname") 0 -1))
;;;; get hosts-dir from hostname
(defvar hosts-dir "~/.config/doom/hosts/")
;;;; actually load the settings of the current host
(load (concat hosts-dir host))

;; modes
;;;; web-mode for tsx files
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; enable fuzzy matching for ivy
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; use ranger.el as default directory handler
(ranger-override-dired-mode t)

;; automatically reload environment on switching buffer
(setq direnv-always-show-summary nil)
(use-package direnv
  :config
  (direnv-mode))

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; configure org-roam
(setq org-roam-directory "~/org/roam")

;; configure org-roam capture templates
;; this is for now not for more than just to separate DnD notes
;; and normal notes from each other via sub-directories
(setq org-roam-capture-templates
      '(("n" "note" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "notes/%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t)
        ("d" "dnd" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "dnd/%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n#+roam_tags: %^{Roam Tag||store|npc|village}\n"
         :unnarrowed t)
        ("c" "dnd npc" plain (function org-roam--capture-get-point)
         "%?\n\n* Information\n* Relations"
         :file-name "dnd/%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n#+roam_tags: npc\n"
         :unnarrowed t)
        ("s" "dnd store" plain (function org-roam--capture-get-point)
         "%?\n\n- Owner: \n\n%+BEGIN_QUOTE\n%+END_QUOTE\n\n* Inventory"
         :file-name "dnd/%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n#+roam_tags: store\n"
         :unnarrowed t)
        ("v" "dnd village" plain (function org-roam--capture-get-point)
         "%?\n\n* Locations\n** Stores\n** Taverns\n** Religious\n** Other\n* Factions\n* History"
         :file-name "dnd/%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n#+roam_tags: village\n"
         :unnarrowed t)))

;; start visual-fill-column-mode when entering org-mode
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(add-hook 'org-mode-hook #'visual-line-mode)

;; configure deft for org
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/"))

;; properly enable org-id
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;; autocomplete id links in org-mode
(defun org-id-complete-link (&optional _arg)
  "Create an id: link using completion"
  (concat "id:"
          (org-id-get-with-outline-path-completion)))

(org-link-set-parameters "id"
                         :complete 'org-id-complete-link)

;; hide encoding when it's the default 'LF UTF-8'
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)
                          (eq buffer-file-coding-system 'prefer-utf-8-unix)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; configure some org-mode related settings
(after! org
  (map! :map org-mode-map
        ;; map inserting quotes, comments, etc. to `SPC m i`
        :n "SPC m i" #'org-insert-structure-template
        ;; start/stop org-roam-server
        :n "SPC n r s" #'org-roam-server-mode)
  (setq ;; prettify org priorities
        org-priority-faces '((?A :foreground "#fb4934")
                             (?B :foreground "#fe8019")
                             (?C :foreground "#fabd2f"))
        ;; customize todo steps
        org-todo-keywords '((sequence "TODO(t)" "REFILE(r)" "INPROGRESS(i)" "WAITING(w)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)"))
        ;; prettify todo steps (gruvbox theme)
        org-todo-keyword-faces
        '(("TODO" :foreground "#fabd2f" :weight bold)
          ("REFILE" :foreground "#fabd2f" :weight bold)
          ("INPROGRESS" :foreground "#d3869b" :weight bold)
          ("WAITING" :foreground "#83a598" :weight bold)
          ("HOLD" :foreground "#fe8019" :weight bold)
          ("DONE" :foreground "#b8bb26" :weight bold)
          ("CANCELLED" :foreground "#928374" :weight bold))
        ;; set the start of the week to Monday
        calendar-week-start-day 1
        org-agenda-start-on-weekday 1))

;; some org-agenda related settings
(map! :after evil-org-agenda
      :map evil-org-agenda-mode-map
      ;; open day view
      :m "d" #'org-agenda-day-view
      ;; open week view
      :m "w" #'org-agenda-week-view)
(setq org-agenda-files '("~/org/" "~/org/roam/dnd/20200901190541-marotoja.org"))

;; prettify priorities in org-mode
(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⬤" "⬤" "⬤")))

;; setup org-roam-server
(use-package! org-roam-server
  :config
  (setq org-roam-server-host "localhost"
        org-roam-server-port 8089
        org-roam-server-authenticate nil
        org-roam-server-network-arrows 'from))
