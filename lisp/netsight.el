;;; netsight.el --- Custom modes -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Matthew Russell

;; Author: Matthew Russell <matthew.russell@horizon5.org>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;;; Code:

(defgroup netsight nil
  "Netsight Development environment"
  :group 'tools
  :prefix "netsight:")

(defgroup netsight-modes nil
  "Modes provided by Netsight"
  :group 'netsight)

(defgroup netsight-keys nil
  "Netsight keybindings."
  :group 'netsight-modes
  :prefix "netsight:key-")

(defvar netsight-keymap
  (let ((map (make-sparse-keymap)))
    ;; Keys for custom netsight defuns
    (define-key map (kbd "C-c C-d") 'netsight-insert-current-date)
    (define-key map (kbd "C-c C-t") 'netsight-insert-current-time)
    (define-key map (kbd "C-c n f") 'netsight-filename-to-clipboard)
    (define-key map (kbd "C-x p") 'netsight-other-window-back)
    (define-key map (kbd "C-c p s") 'netsight-insert-package-summary)
    (define-key map (kbd "C-x W") 'netsight-fix-horizontal-size)
    (define-key map (kbd "<kp-5>") 'netsight-insert-debug)
    (define-key map (kbd "<kp-9>") 'netsight-start-ide-mode)
    (define-key map (kbd "<next>") 'netsight-ne-page-dn)
    (define-key map (kbd "<prior>") 'netsight-ne-page-up)

    ;; Overrides for builtin commands
    (define-key map (kbd "M-c") 'capitalize-word)
    (define-key map (kbd "M-d") 'netsight-delete-word)
    (define-key map (kbd "M-g f") 'list-faces-display)
    (define-key map (kbd "M-s x") 'replace-regexp)
    (define-key map (kbd "M-z") 'goto-char)
    (define-key map (kbd "C-x C-o") 'ffap)
    (define-key map (kbd "C-x r r") 'revert-buffer)
    (define-key map (kbd "C-x w") 'woman)
    (define-key map (kbd "<kp-0>") 'goto-line)
    (define-key map (kbd "<kp-1>") 'delete-other-windows)
    (define-key map (kbd "<kp-2>") 'split-window-horizontally)
    (define-key map (kbd "<kp-3>") 'call-last-kbd-macro)
    (define-key map (kbd "<kp-4>") 'indent-region)
    (define-key map (kbd "<kp-6>") 'comment-region)

    ;; VC command aliases
    (define-key map (kbd "C-x c i") 'vc-next-action)

    ;; re-map standard delete options
    (define-key map (read-kbd-macro "<C-backspace>") 'backward-kill-word)
    (define-key map (read-kbd-macro "<M-DEL>") 'backward-kill-word)
    map)
  "Keymap for `netsight-keys'.")

;;;###autoload
(define-minor-mode netsight-mode
  "Toggle Netsight mode.

\\{netsight-keymap}
Interactively with no argument, this command toggles the mode.
When activated, this mode makes the 'standard' netsight keybindings
take effect."
  nil
  :lighter " Netsight"
  :keymap netsight-keymap
  :group 'netsight-keys)

(define-globalized-minor-mode global-netsight-mode
  netsight-mode
  netsight-mode
  :group 'netsight)

(defconst netisght-devtools-directory "/home/dev/devtools"
  "The directory where the Netsight devtools source is checked out.")

;; Set misc settings.
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

;; Not sure this is safe or not, disabling for now...
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

;; Un-disable some 'dangerous!' commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'netsight)
;;; netsight.el ends here
