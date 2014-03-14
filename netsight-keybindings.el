;;; netsight-keybindings.el --- Default keybindings for standard and custom functions.

;;; Commentary:
;; 

;;; Code:

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
      '("Goto bookmark" . bookmark-jump))

(global-set-key "\C-c\C-d" 'insert-current-date)
(global-set-key "\C-c\C-t" 'insert-current-time)
(global-set-key (kbd "C-x C-o") 'ffap)
(global-set-key (kbd "M-s x") 'replace-regexp)
(global-set-key (kbd "M-g f") 'list-faces-display)
(global-set-key (kbd "M-n f") 'filename-to-clipboard)
(global-set-key (kbd "C-x w") 'woman)
(global-set-key (kbd "M-c") 'capitalize-word)
(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(global-set-key (read-kbd-macro "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "C-x W") 'fix-horizontal-size)
(global-set-key "\C-xp" 'other-window-back)
(global-set-key [prior] 'ne-page-up)
(global-set-key [next] 'ne-page-dn)
(global-set-key [kp-0] 'goto-line)
(global-set-key [kp-1] 'delete-other-windows)
(global-set-key [kp-2] 'split-window-horizontally)
(global-set-key [kp-3] 'call-last-kbd-macro)
(global-set-key [kp-4] 'indent-region)
(global-set-key [kp-5] 'insert-debug)
(global-set-key [kp-6] 'comment-region)
(global-set-key [kp-7] 'flymake-goto-next-error)
(global-set-key [kp-8] 'browse-kill-ring)
(global-set-key [kp-9] 'start-ide-mode)

(global-set-key "\C-xrr" 'revert-buffer)
(global-set-key "\C-xci" 'vc-next-action)

(provide 'netsight-keybindings)

;;; netsight-keybindings.el ends here
