;;; netsight-dired.el --- Customisation of `dired-mode'.

;;; Commentary:
;; 

;;; Code:
(require 'dired-x)

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding mark."
  (sort-directories-first))

;; This is a buffer-local variable
(setq-default dired-omit-files-p t)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))

;; Use elisp ls so that dired works from emacsclient externally :)
(defvar ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

(setq-default dired-listing-switches "-laFGgD")

(provide 'netsight-dired)

;;; netsight-dired.el ends here
