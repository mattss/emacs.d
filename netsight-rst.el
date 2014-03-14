;;; netsight-rst.el --- Default settings and hooks for editing reStructuredText

;;; Commentary:
;;; Given that themse are user dependent, it's quite possible that these styles
;;; don't look great in your theme. 
;;; Use the variables defined below as guide for what to customize.
;;; Use M-x `customize-face' ENTER "rst" ENTER to configure.
;; 

(require 'rst)
;;; Code:

(setq rst-adornment-faces-alist
      (quote ((nil . font-lock-keyword-face)
              (nil . font-lock-keyword-face)
              (nil . rst-level-1-face)
              (2 . rst-level-2-face)
              (3 . rst-level-3-face)
              (4 . rst-level-4-face)
              (5 . rst-level-5-face)
              (nil . rst-level-5-face))))
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

(provide 'netsight-rst)

;;; netsight-rst.el ends here
