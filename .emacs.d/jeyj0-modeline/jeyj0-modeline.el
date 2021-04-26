; simple modeline configuration, with heavy inspiration from simple-modeline

(defgroup jeyj0-modeline nil
  "A simple mode line"
  :prefix "jeyj0-modeline-"
  :group 'mode-line)

(defvar jeyj0-modeline--default-mode-line mode-line-format)

(defcustom jeyj0-modeline-segments
  '(jeyj0-modeline-segment-modified
    jeyj0-modeline-segment-buffer-name
    jeyj0-modeline-segment-separator
    jeyj0-modeline-segment-position
    jeyj0-modeline-segment-separator
    jeyj0-modeline-segment-major-mode
    jeyj0-modeline-segment-separator
    jeyj0-modeline-segment-vc)
  "Modeline segments")

(defface jeyj0-modeline-status-modified
  '((t (:inherit (font-lock-variable-name-face))))
  "Face for the 'modified' indicator symbol in the mode-line")

(defface jeyj0-modeline-status-error
  '((t (:inherit (error))))
  "Face for error status indicators in the mode-line")

(defface jeyj0-modeline-unimportant
  '((t (:inherit (shadow))))
  "Face for less important elements in the mode-line")

(defun jeyj0-modeline--format ()
  "Return a string from a list of SEGMENTS"
  (format-mode-line (mapcar
		     (lambda (segment)
		       `(:eval (,segment)))
		     jeyj0-modeline-segments)))

(defun jeyj0-modeline-make-mouse-map (mouse function)
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

(defun jeyj0-modeline-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
      (let* ((read-only (and buffer-read-only (buffer-file-name)))
	     (modified (buffer-modified-p)))
	(propertize
	 (if read-only " ●" (if modified " ●" " ○"))
	 'face `(:inherit
		 ,(if modified 'jeyj0-modeline-status-modified
		    (if read-only 'jeyj0-modeline-status-error
		      'jeyj0-modeline-unimportant)))
	 'help-echo (format
		     "Buffer is %s and %smodified\nmouse-1: Toggle read-only status."
		     (if read-only "read-only" "writable")
		     (if modified "" "not "))
	 'local-map (purecopy (jeyj0-modeline-make-mouse-map
			       'mouse-1
			       (lambda (event)
				 (interactive "e")
				 (with-selected-window (posn-window (event-start event))
				   (read-only-mode 'toggle)))))
	 'mouse-face 'mode-line-highlight))))

(defun jeyj0-modeline-segment-buffer-name ()
  (propertize " %b" 'face 'mode-line-buffer-id))

(defun jeyj0-modeline-segment-separator ()
  (propertize
   " |"
   'face 'jeyj0-modeline-unimportant))

(defun jeyj0-modeline-segment-position ()
  `((line-number-mode
     ((column-number-mode
       (column-number-indicator-zero-based
	(8 " %l:%c")
	(8 " %l:%C"))
       (5 " L%l")))
     ((column-number-mode
       (column-number-indicator-zero-based
	(5 " C%c")
	(5 " C%C")))))
    ,(if (region-active-p)
	 (propertize (format "+%s"
			     (apply #'+ (mapcar
					 (lambda (pos)
					   (- (cdr pos)
					      (car pos)))
					 (region-bounds))))
		     'font-lock-face 'font-lock-variable-name-face))))

(defun jeyj0-modeline-segment-major-mode ()
  (propertize
   (concat " "
	   (or (and (boundp 'delighted-modes)
		    (cadr (assq major-mode delighted-modes)))
	       (format-mode-line mode-name)))))

(defun jeyj0-modeline-segment-vc ()
  '(vc-mode vc-mode))

(defvar jeyj0-modeline--mode-line
  '((:eval
     (jeyj0-modeline--format))))

(define-minor-mode jeyj0-modeline-mode
  "Minor mode to get a simple mode line."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'jeyj0-modeline
  :global t
  (if jeyj0-modeline-mode
      (progn
	;; Set the new mode-line-format
	(setq-default mode-line-format '(:eval jeyj0-modeline--mode-line)))
    (progn
      ;; Restore the original mode-line format
      (setq-default mode-line-format jeyj0-modeline--default-mode-line))))
