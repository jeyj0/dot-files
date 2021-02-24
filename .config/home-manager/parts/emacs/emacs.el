;; -*- lexical-binding: t; -*-
;;; ^ that's for performance reasons

;; do not show splash screen
(setq inhibit-startup-screen t)

;; use use-package
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))

;; disable default emacs visuals
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

;; enable some included things
(show-paren-mode 1) ; highlight matching parenthesis

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; try preventing emacs from automatically splitting the window
;(set-frame-parameter nil 'unsplittable t)
;(setq pop-up-frames 'graphic-only)

;; make nix-installed packages available
(package-initialize)

;; set up backup files (in a reasonable way)
(setq version-control t
      backup-by-copying t
      delete-old-versions t
      vc-make-backup-files t
      backup-directory-alist '(("." . "~/.emacsbackups/")))

;; visual packages
;;; merge modeline and minibuffer, for a more minimalistic look
;(use-package mini-modeline
;  :after smart-mode-line
;  :init
;  (setq mini-modeline-display-gui-line t)
;  :config
;  (mini-modeline-mode nil))
;;; make modeline have a fixed width, mostly for mini-modeline to look good
;(use-package smart-mode-line
;  :init
;  (setq sml/shorten-directory t)
;  (setq sml/shorten-modes t)
;  (setq sml/name-width 20)
;  (setq sml/mode-width 20))

;; add direnv support
(use-package direnv
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

;; enable evil-mode globally
(use-package evil
  :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-respect-visual-line-mode t)
    (setq evil-shift-width 2) ; I prefer 2-space indentation
    (setq evil-move-cursor-back nil) ; this fixes vim's weird behavior
  :config
    (evil-mode 1))

;;; evil-collection for better evil support in other modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;; enable evil-surround, the equivalent of Tim Pope's surround.vim
(use-package evil-surround
  :config
    (global-evil-surround-mode))

;; help me learn shortcuts
(use-package which-key
  :config
    (which-key-mode))

;; enable visual-fill-column-mode in org-mode
(use-package visual-fill-column
  :config
    (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
    (add-hook 'org-mode-hook #'visual-line-mode))

;;; configure org-roam
(use-package org-roam
  :init
    (setq my-org-roam-directory (file-truename "~/org/roam"))
    (make-directory my-org-roam-directory t)
    (setq org-roam-directory my-org-roam-directory)
    ;;;; org-roam links completion-at-point
    (setq org-roam-completion-everywhere t)
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
	    ("s" "dnd session" plain (function org-roam--capture-get-point)
            "%?"
            :file-name "dnd/%<%Y%m%d%H%M%S>-${slug}"
            :head "#+TITLE: ${title}\n"
            :unnarrowed t)
	    ("c" "dnd npc" plain (function org-roam--capture-get-point)
	    "%?\n\n* Information\n* Relations"
	    :file-name "dnd/%<%Y%m%d%H%M%S>-${slug}"
	    :head "#+TITLE: ${title}\n#+roam_tags: npc\n"
	    :unnarrowed t))))


;; enable autocompletion using company-mode

;;; don't open company completion popup in unsupported evil states
;;;; vgl. https://github.com/emacs-evil/evil-collection/blob/master/modes/company/evil-collection-company.el
(defcustom evil-company-supported-states '(insert replace emacs)
  "The `evil-state's which `company' function can be requrested."
  :type '(repeat symbol))
(defun company-supported-p (command &rest _)
  "Return non-nil if `evil-state' is in supported states."
  (cond
   ((not (bound-and-true-p evil-mode)) t)
   ((eq command 'prefix)
    (memq evil-state evil-company-supported-states))
   (t t)))
(advice-add 'company-call-backend :before-while 'company-supported-p)

(use-package company
  :init
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
  :config
    (add-hook 'after-init-hook 'global-company-mode)
    (advice-add 'company-call-backend :before-while 'company-supported-p))

;; configure org-mode
(use-package org
  :init
    ;;; set directory containing org files
    (setq org-directory "~/org/")
    ;;; set up org-agenda
    (setq org-agenda-files '("~/org/" "~/org/journal/"))
    ;;; stop agendas from being built at startup
    ;;; I only need them when I call them
    (setq org-agenda-inhibit-startup t)
    ;;; do not open a new split when opening org links
    (add-to-list 'org-link-frame-setup '(file . find-file))
    ;;; allow image size to be configured in org via ATTR_ORG
    (setq org-image-actual-width nil)
    ;;; attachments
    (setq org-attach-directory "~/org/attachments/")
    ;;; set tags to be only space-separated after todo headlines
    (setq org-tags-column 0)
    ;;; configure tags that are available everywhere
    (setq org-tag-alist '(("private" . ?p) ("code" . ?c) ("dnd" . ?d) ("work" . ?w)))
    ;;; set valid TODO states
    (setq org-todo-keywords
	  '((sequence "TODO(t)" "WAITING(w)" "DOING(i)" "|" "DONE(d)" "CANCELLED(c)" "CARRIED")))
    ;;; hide asterisks, slashes,... that mark text as italic/bold/...
    (setq org-hide-emphasis-markers t))

;; add evil-org-mode package, a package to improve evil and org-mode integration
(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; setup org-journal for fleeting note-taking and personal organization
;;; function to format the header of a journal file, modified from org-journal's readme
(defun my-org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily (format-time-string "#+TITLE: %A, %d %B %Y (Daily Journal)\n#+STARTUP: show2levels\n\n" time))
      (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded\n\n")
      (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded\n\n")
      (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded\n\n"))))

;;; function to tag carried-over items instead of deleting them
;;; taken from org-journal's readme
(defun my-org-journal-carryover-item-handler (old_carryover)
  (save-excursion
    (let ((matcher (cdr (org-make-tags-matcher org-journal-carryover-items))))
      (dolist (entry (reverse old_carryover))
	(save-restriction
	  (narrow-to-region (car entry) (cadr entry))
	  (goto-char (point-min))
	  (org-scan-tags '(lambda ()
			    (org-todo "CARRIED"))
			 matcher org--matcher-tags-todo-only))))))

(use-package org-journal
  :init
  (setq org-journal-dir "~/org/journal")
  (setq org-journal-find-file 'find-file)
  (setq org-journal-date-format "%Y-%m-%d (%A)")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-file-header 'my-org-journal-file-header-func)
  (setq org-journal-carryover-items "TODO=\"TODO\"|TODO=\"WAITING\"|TODO=\"DOING\"")
  (setq org-journal-handle-old-carryover 'my-org-journal-carryover-item-handler))

;; use ivy for minibuffer-completion
(use-package flx)
(use-package ivy
  :init
    ;;; is recommended for beginners
    (setq ivy-use-virtual-buffers t)
    ;;; enable fuzzy matching
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
    ;;; make the input a valid selection
    (setq ivy-use-selectable-prompt t)
  :config
    (ivy-mode 1))

;; color theme to gruvbox for each new frame (otherwise creates issues)
(use-package doom-themes
  :init
    (setq doom-themes-enable-bold 5
          doom-themes-enable-italic t)
    (load-theme 'doom-gruvbox t)
    (doom-themes-org-config))
(defun set-theme-hook-func (frame)
  (load-theme 'doom-gruvbox t)
  (scroll-bar-mode 0))
(add-hook 'after-make-frame-functions 'set-theme-hook-func)

;; magit
(use-package magit)

;; language modes
;;; haskell
(use-package haskell-mode)
(use-package lsp-haskell
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

;;; nix
(use-package nix-mode
  :mode "\\.nix\\'")

;; language server protocol
(use-package lsp-ui)
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-completion-provider :capf)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  :hook
  (haskell-mode . lsp-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

;; set font
(add-to-list 'default-frame-alist '(font . "Hack-12"))

;;; KEYMAPS
;;;; custom functions for keymaps

(defun my-next-user-buffer ()
  "Switch to the next user buffer.
user buffer is determined by `my-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (my-user-buffer-q))
	  (progn (next-buffer)
		 (setq i (1+ i)))
	(progn (setq i 100))))))

(defun my-prev-user-buffer ()
  "Switch to the previous user buffer.
user buffer is determined by `my-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (my-user-buffer-q))
	  (progn (previous-buffer)
		 (setq i (1+ i)))
	(progn (setq i 100))))))

(defun my-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This functio is used by buffer switching command and close buffer command, so
that next buffer shown is a user buffer.
You can override this function to get your idea of 'user buffer'.
Version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
	nil
      (if (string-equal "magit" (substring (buffer-name) 0 5))
	  nil
	t
      ))))

(defun my-kill-current-buffer ()
  "Kill the current buffer and switch to a user buffer."
  (interactive)
  (kill-current-buffer)
  (my-next-user-buffer))

;;;; I don't use evil's leader functionality, as it isn't as flexible as emacs prefix bindings
(define-prefix-command 'jeyj0-leader-map)

(defun map-leader (keys cmd)
  "Map keys to command after leader key"
  (define-key jeyj0-leader-map (kbd keys) cmd))

;;;;; setting SPC as main leader key
(define-key evil-normal-state-map (kbd "SPC") jeyj0-leader-map)
(define-key evil-visual-state-map (kbd "SPC") jeyj0-leader-map)

;;;;;; various useful leader mappings
(map-leader "x" 'execute-extended-command)
(map-leader ":" 'eval-expression)

;;;;;; file manipulation
(map-leader "SPC" 'project-find-file)
(map-leader "fs" 'save-buffer)

;;;;;; buffer manipulation
(map-leader "bk" 'my-kill-current-buffer)
(map-leader "bh" 'my-next-user-buffer)
(map-leader "bl" 'my-prev-user-buffer)

;;;;;; git (magit)
(map-leader "g" 'magit)

;;;;;; notes
(map-leader "nf" 'org-roam-find-file)
(map-leader "ni" 'org-roam-insert)
(map-leader "ns" 'org-todo)
(map-leader "na" 'org-agenda)
(map-leader "nt" 'org-set-tags-command)
(map-leader "nr" 'org-roam)
;;;;;;; display settings
(map-leader "ndi" 'org-toggle-inline-images)
(map-leader "ndl" 'org-toggle-link-display)

;;;;;; window manipulation
(map-leader "wh" 'evil-window-left)
(map-leader "wj" 'evil-window-down)
(map-leader "wk" 'evil-window-up)
(map-leader "wl" 'evil-window-right)
(map-leader "ws" 'evil-window-split)
(map-leader "wv" 'evil-window-vsplit)

;;;;;; org-mode-mappings
(evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

;;; esc quits everything, as expected from using vim
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;;;; taken from https://stackoverflow.com/questions/8483182/evil-mode-best-practice
