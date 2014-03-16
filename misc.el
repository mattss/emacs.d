;;; netsight-style.el --- Default settings across a number of different modes. -*- lexical-binding: t -*-
;;; Buffer Encoding

;;; Commentary:
;;
;;; Code:

;; These should not be overriden unless their is a very good reason.
;; e.g You write code in a language whose symbols are not represented
;; by the UTF-8 character set.
(defconst netisght-devtools-directory "/home/dev/devtools"
  "The directory where the Netsight devtools source is checked out.")

;; Input
(setq-default indent-line-function 'insert-tab)
(setq indent-tabs-mode nil)
(setq tab-always-indent nil)
    
;; scrolling - do not add newlines when cursoring past last line in file
(setq scroll-step 1)
(setq next-line-add-newlines nil)

;; Display
(global-linum-mode 0)
(setq fill-column 79)
(setq transient-mark-mode 't)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq search-highlight t)
(setq query-replace-highlight t)


;; Not sure this is safe of ney
;;; disabling for now
;; (setq-default kill-buffer-query-functions
;;   (remq 'process-kill-buffer-query-function
;;          kill-buffer-query-functions))

;;; Desktop mode
;;; Useful for remembering a set of file you're working on -
;;;  - enables switching between projects and keeping state.
(setq desktop-save-mode t)

;; Misc settings
(setq mail-interactive t)

;; Annoyance factor
(fset 'yes-or-no-p 'y-or-n-p)
(setq redisplay-dont-pause 't)
(setq font-lock-verbose nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

;; Un-disable some 'dangerous!' commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;; misc.el ends here

