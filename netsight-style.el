;;; netsight-style.el --- Default settings across a number of different modes.
;;; Buffer Encoding

;;; Commentary:
;;; The default settings here may be overriden
;;; in `netsight-custom-config-filename', but some things should
;;; not be changed unless you really know what you are doing.
;; 

;;; Code:

(defvar netsight-custom-config-filename "custom.emacs"
       "Filename read after the netsight package has been loaded.
Provides the ability to override.")


;; These should not be overriden unless their is a very good reason.
;; e.g You write code in a language whose symbols are not represented
;; by the UTF-8 character set.
(set-language-environment 'utf-8)
(setq-default locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Input
(setq-default indent-line-function 'insert-tab)

;; All about tabs.
;; We KILL ALL TABS - tabs are evil.
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent nil)
(setq-default tab-width 4)

;; scrolling - do not add newlines when cursoring past last line in file
(setq scroll-step 1)


(setq-default next-line-add-newlines nil)

;; Display
(global-linum-mode 0)
(setq-default default-fill-column 79)
(setq-default shell-prompt-pattern "\\u@\\h: \\w $ ")
(setq-default transient-mark-mode 't)
(setq-default column-number-mode t)
(setq-default scroll-bar-mode nil)
(setq-default inhibit-startup-message t)
(setq-default inhibit-splash-screen t)
(setq-default inhibit-startup-echo-area-message t)
(setq-default search-highlight t)
(setq-default query-replace-highlight t)
(setq-default kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;;; Desktop mode
;;; Useful for remembering a set of file you're working on -
;;;  - enables switching between projects and keeping state.
(setq-default desktop-save-mode 't)

;; Misc settings
(setq-default major-mode 'text-mode)
(setq-default mail-interactive 't)

;; Ediff
(setq-default ediff-shell (getenv "$SHELL"))
(setq-default ediff-split-window-function (quote split-window-vertically))

(eval-after-load 'diff-mode
  '(define-key diff-mode-shared-map (kbd "M-x v =")
     'ediff-current-buffer-revision))

;; Annoyance factor
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default redisplay-dont-pause 't)
(setq-default font-lock-verbose nil)
(setq-default confirm-nonexistent-file-or-buffer nil)
(setq-default ido-create-new-buffer 'always)

;; Un-disable some 'dangerous!' commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

; Zope/Plone modes
(setq-default sgml-basic-offset 2)
(setq-default auto-mode-alist (append '(("\\.pt$" . sgml-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.cpt$" . sgml-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.zcml$" . sgml-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.cpy$" . python-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.vpy$" . python-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.js.dtml$" . java-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.po$" . text-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.kss$" . css-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.css.dtml$" . css-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.kss$" . css-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.zsql$" . sql-mode)) auto-mode-alist))
(setq-default auto-mode-alist (append '(("\\.el$" . emacs-lisp-mode)) auto-mode-alist))

(setq auto-mode-alist (append '(("~/.emacs" . emacs-lisp-mode))
                              auto-mode-alist))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)

(provide 'netsight-style)

;;; netsight-style.el ends here
