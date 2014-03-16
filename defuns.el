;;; defuns.el --- Utility functions -*- lexical-binding: t; -*-
;; lispy stuff

;;; Commentary:
;; 

;;; Code:

(defun log-if-loaded (pkgname)
  "Print a message after some package has been loaded.
Argument PKGNAME The name of the package being loaded."
  (eval-after-load pkgname
    (message (format "Loaded %s" pkgname))))

(defun arrange-frame (w h x y)
  "Set the width, height, and X/y position of the current frame."
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))


; M-Del should delete (not kill/cut) word
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
Argument ARG Word to be deleted."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))


(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times.
Argument ARG Word to be deleted."
  (interactive "p")
  (delete-word (- arg)))

;; 80 char window resizing
(defun fix-frame-horizontal-size (width)
  "Set the frame's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (if window-system
      (set-frame-width (selected-frame) (or width 80))
    (error "Cannot resize frame horizontally: is a text terminal")))


(defun fix-window-horizontal-size (width)
  "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (enlarge-window (- (or width 80) (window-width)) 'horizontal))


(defun fix-horizontal-size (width)
  "Set the window's or frame's width to 80 (or prefix arg WIDTH)."
  (interactive "P")
  (condition-case nil
      (fix-window-horizontal-size width)
    (error
     (condition-case nil
	 (fix-frame-horizontal-size width)
       (error
	(error "Cannot resize window or frame horizontally"))))))


; TODO: remove me, this is in a library somewhere.
; util to get result of shell command
(defun shell-command-to-string (command)
  "Execute shell command COMMAND and return its output as a string."
  (with-output-to-string
    (with-current-buffer standard-output
      (call-process shell-file-name nil t nil shell-command-switch command))))

; Page up/down functionality
(defun ne-page-up ()
  "Move point to top of screen if it is within the window, else top of prev screen."
  (interactive)
  (if (= (window-point)(window-start))
      (scroll-down)
       nil)
  (sit-for 0)
  (set-window-point nil (window-start)))

(defun ne-page-dn ()
  "Move point to bottom of screen if within window, else move to bottom of next screen."
  (interactive)
  (if (= (window-point)(- (window-end) 1))
      (scroll-up)
      nil)
  (sit-for 0)
  (set-window-point nil (- (window-end) 1)))

;; Quick-Insert python debug mode
(defun insert-debug ()
  "Insert python debug commands."
  (interactive)
  (insert "import pdb; pdb.set_trace()")
  (backward-char 27) )

(defun start-ide-mode ()
  "Set up 3 column layout with shell."
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (other-window 2)
  (shell))

(defun other-window-back ()
 "Move to previous window."
 (interactive)
 (other-window -1))

;; Confirm switch to non-existent buffer
(defadvice switch-to-buffer (around confirm-non-existing-buffers activate)
  "Switch to non-existing buffers only upon confirmation."
  (interactive "BSwitch to buffer: ")
  (if (or (get-buffer (ad-get-arg 0))
          (y-or-n-p (format "´%s' does not exist, create? "(ad-get-arg 0))))
      ad-do-it))

; Igor
(defun igor-init ()
  "Insert python debug commands."
  (interactive)
  (call-process "igor" nil t nil buffer-file-name)
  (revert-buffer 'ignore-auto 'dont-ask 'preserve-modes))

; Gitweb
(defun gitweb ()
  "Insert python debug commands."
  (interactive)
  (call-process "gitweb" nil 0 nil buffer-file-name))

; nspaste
(defun nspaste-init ()
 "Insert python debug commands."
 (interactive)
 (let (pos1 pos2 bds)
   (if (and transient-mark-mode
	    mark-active)
       (setq pos1 (region-beginning) pos2 (region-end))
     (progn
       (setq bds (bounds-of-thing-at-point 'symbol))
       (setq pos1 (car bds) pos2 (cdr bds))))
   (call-process "nspaste" nil 0 nil (buffer-substring pos1 pos2))))


(defun filename-to-clipboard ()
  "Put the absolute path to the current file name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


(defun buffer-name-to-clipboard ()
  "Put the current buffer name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-current-date'.
See documentation of `format-time-string' for possible replacements")


(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")


(defun insert-current-date ()
  "Insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

(defun insert-current-time ()
  "Insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

(defun sort-directories-first ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defun flymake-pyflakes-init ()
  "Create a temporary buffer for external pychecker command."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list flymake-python-pyflakes-executable (list local-file))))

(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: ")))))

(defun message-fmt (msg arg)
  (message (format msg arg)))

(defun insert-package-summary ()
  "Inserts the summary of an ELPA package at point."
  (interactive)
  (let ((pkg-name     
         (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
            (read-string "Package: "))))
    (message (format "Looking up summary for package %s" pkg-name))
    (let* ((pkg (intern pkg-name))
           (pkg-vec (assq pkg package-alist))
           (pkg-desc-v (cdr pkg-vec))
           (pkg-desc-vlen (length pkg-desc-v)))
      (if (eq pkg-desc-v nil)   
          (message "Could not find package info"))
      (when pkg-desc-vlen
        (let ((summary (elt pkg-desc-v (- pkg-desc-vlen 1))))
          (insert summary))))))

(defun load-experiment (file-name)
  "Loads an experimental elisp file into the current session."
  (let* ((exp-name (locate-user-emacs-file "experimental"))
	 (exp-dir (file-name-as-directory exp-name))
	 (exp-path (concat exp-dir file-name)))
    (if (file-exists-p exp-path)
	(load exp-path)
      (message 
       (format "Could not find experimental elisp %s" exp-path)))))
  


