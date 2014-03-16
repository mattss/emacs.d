;;; init.el --- Init file for Netsight development. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Initialize Emacs configuration.
;;

;;; Code:
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "experimental"))

;; Turn off UI clutter
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;; Setup package management
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(package-initialize)
(require 'pallet)
(require 'f)
(require 's)
(require 'use-package)

;; Ensure PATH is preserved from shell.
(exec-path-from-shell-initialize)

;; Load local elisp
(defun load-local (file)
  "Load a local FILE."
  (load (f-expand file user-emacs-directory)))
(load-local "defuns.el")
(load-local "misc.el")

;;; custom user Lisp (from template on first load)
(setq custom-file "~/custom.emacs")
(unless (file-exists-p custom-file)
  (with-current-buffer (get-buffer-create "custom-file")
    (insert-file-contents (locate-user-emacs-file "user-custom-file-template.el") nil 0)
     (write-region (buffer-string) nil custom-file)))
(load custom-file)
  
;; Package configuration
(use-package bookmark
  :init
  (progn
    (define-key global-map [menu-bar bookmarks]
      (cons "Bookmarks" (make-sparse-keymap "Bookmarks")))
    (define-key global-map
      [menu-bar bookmarks bookmark-insert]
      '("Insert bookmark into buffer" . bookmark-insert))
    (define-key global-map
      [menu-bar bookmarks bookmark-delete]
      '("Delete bookmark" . bookmark-delete))
    (define-key global-map
      [menu-bar bookmarks bookmark-save]
      '("Save bookmarks" . bookmark-save))
    (define-key global-map
      [menu-bar bookmarks list-bookmarks]
      '("List bookmarks" . list-bookmarks))
    (define-key global-map
      [menu-bar bookmarks bookmark-set]
      '("Add bookmark" . bookmark-set))
    (define-key global-map
      [menu-bar bookmarks bookmark-jump]
      '("Goto bookmark" . bookmark-jump))))

(use-package css-mode
  :mode (("\\.kss$" . css-mode)
         ("\\.css.dtml$". css-mode)))

(use-package dired
  :config
  (progn
    (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
      "Sort dired listings with directories first before adding mark."
      (sort-directories-first))))

(use-package dired-x
  :config
  (progn
    (setq-default dired-omit-files-p nil)
    (setq dired-omit-files
          (concat dired-omit-files "\\|^\\..+$"))))

(use-package ediff
  :config
  (progn
    (setq ediff-shell (getenv "$SHELL"))
    (setq-default ediff-split-window-function
                  (quote split-window-vertically))))

(use-package flycheck
  :bind ("<kp-7>" . flycheck-next-error)
  :config
  (progn
    (setq flycheck-python-flake8-executable "pycheckers.py")
    (setq flycheck-highlighting-mode (quote lines)))
  :init
  (progn
    (fringe-mode (quote (4 . 0)))
    (global-flycheck-mode)))
    
(use-package git-gutter+)

(use-package java-mode
  :mode (("\\.js.dtml$" . java-mode)))

(use-package ls-lisp
  :config (setq ls-lisp-use-insert-directory-program nil))

(use-package mule
  :config (setq locale-coding-system 'utf-8)
  :init
  (progn
    (set-language-environment 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)))

(use-package nxml-mode
  :config (setq nxml-child-indent 2)
  :mode (("\\.xml$" . nxml-mode)
         ("\\.zcml$" . nxml-mode)))

(use-package python-mode
  :mode (("\\.py$" . python-mode)
         ("\\.cpy$" . python-mode)
         ("\\.vpy$" . python-mode)))


(use-package rst
  :config
  (progn
    (setq rst-adornment-faces-alist
          (quote ((nil . font-lock-keyword-face)
                  (nil . font-lock-keyword-face)
                  (nil . rst-level-1-face)
                  (2 . rst-level-2-face)
                  (3 . rst-level-3-face)
                  (4 . rst-level-4-face)
                  (5 . rst-level-5-face)
                  (nil . rst-level-5-face))))))

(use-package sass-mode
  :config (setq sass-indent-offset 2))

(use-package sendmail
  config: (setq-default mail-interactive 't))

;; Emacs server configuration
;; Allows use with screen
;; Start either gnuserv or emacsserver for external access
(use-package server
  :config
  (progn
    (setq server-socket-dir
          (format "%semacs%d"
                  temporary-file-directory
                  (user-uid)))
    (setq server-use-tcp 't))
  :init
  (progn
    (when (not (or
                (window-system)
                (eq 'windows-nt system-type)))
      (server-start))))

(use-package sgml-mode
  :config (setq sgml-basic-offset 2)
  :mode (("\\.pt$" . sgml-mode)
         ("\\.cpt$" . smgl-mode)
         ("\\.html" . sgml-mode)
         ("\\.htm" . sgml-mode)))

(use-package shell
  config: (setq shell-prompt-pattern "\\u@\\h: \\w $ "))

(use-package sql-mode
  :mode (("\\.zsql$" . sql-mode)))

(use-package text
  :mode (("\\.po$" . text-mode)
	 ("\\.pot$" . text-mode)))
  
(use-package vc)

;; key-bindings
(bind-key "C-c C-d" 'insert-current-date)
(bind-key "C-c C-t" 'insert-current-time)
(bind-key (kbd "C-x C-o") 'ffap)
(bind-key (kbd "M-s x") 'replace-regexp)
(bind-key (kbd "M-g f") 'list-faces-display)
(bind-key (kbd "M-n f") 'filename-to-clipboard)
(bind-key (kbd "C-x w") 'woman)
(bind-key (kbd "M-c") 'capitalize-word)
(bind-key (read-kbd-macro "<M-DEL>") 'backward-kill-word)
(bind-key (read-kbd-macro "<C-backspace>") 'backward-kill-word)
(bind-key "C-x W" 'fix-horizontal-size)
(bind-key "C-x p" 'other-window-back)
(bind-key [prior] 'ne-page-up)
(bind-key [next] 'ne-page-dn)
(bind-key [kp-0] 'goto-line)
(bind-key [kp-1] 'delete-other-windows)
(bind-key [kp-2] 'split-window-horizontally)
(bind-key [kp-3] 'call-last-kbd-macro)
(bind-key [kp-4] 'indent-region)
(bind-key [kp-5] 'insert-debug)
(bind-key [kp-6] 'comment-region)
(bind-key [kp-8] 'browse-kill-ring)
(bind-key [kp-9] 'start-ide-mode)
(bind-key "C-x r r" 'revert-buffer)
(bind-key "C-x c i" 'vc-next-action)
(bind-key "M-z" 'goto-char)

;;; init.el ends here
