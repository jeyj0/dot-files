;; -*- lexical-binding: t; -*-
;;; ^ that's for performance reasons

;; use use-package
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))

;; disable default emacs visuals
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar 0)
(blink-cursor-mode 0)

;; enable some included things
(show-paren-mode 1) ; highlight matching parenthesis

;; make nix-installed packages available
(package-initialize)

;; enable evil-mode globally
(use-package evil
  :init
    (setq evil-respect-visual-line-mode t)
    (setq evil-shift-width 2) ; I prefer 2-space indentation
    (setq evil-move-cursor-back nil) ; this fixes vim's weird behavior
  :config
    (evil-mode 1))
;;; enable evil-surround, the equivalent of Tim Pope's surround.vim
(use-package evil-surround
  :config
    (global-evil-surround-mode))

;; enable visual-fill-column-mode in org-mode
(use-package visual-fill-column
  :config
    (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
    (add-hook 'org-mode-hook #'visual-line-mode))

;;; configure org-roam
(use-package org-roam
  :init
    (setq org-roam-directory (file-truename "~/org/roam"))
    ;;;; org-roam links completion-at-point
    (setq org-roam-completion-everywhere t))

;; enable autocompletion using company-mode
(use-package company
  :init
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1))

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

(add-hook 'after-init-hook 'global-company-mode)

;; configure org-mode
(use-package org
  :init
    ;;; set directory containing org files
    (setq org-directory "~/org/")
    ;;; do not open a new split when opening org links
    (add-to-list 'org-link-frame-setup '(file . find-file))
    ;;; hide asterisks, slashes,... that mark text as italic/bold/...
    (setq org-hide-emphasis-markers t))

;; use ivy for completion
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

;; color theme to gruvbox
(use-package doom-themes
  :init
    (setq doom-themes-enable-bold 5
          doom-themes-enable-italic t)
    (load-theme 'doom-gruvbox t)
    (doom-themes-org-config))

;; set font
(add-to-list 'default-frame-alist '(font . "Hack-12"))

;;; KEYMAPS
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

;;;;;; file manipulation
(map-leader "SPC" 'project-find-file)
(map-leader "fs" 'save-buffer)

;;;;;; buffer manipulation
(map-leader "bk" 'kill-current-buffer)
(map-leader "bj" 'evil-next-buffer)
(map-leader "bk" 'evil-prev-buffer)

;;;;;; notes
(map-leader "nf" 'org-roam-find-file)
(map-leader "ni" 'org-roam-insert-immediate)

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
