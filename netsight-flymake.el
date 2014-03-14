;; netsight-flymake.el --- Provides the default flymake integration for development.
;; Flymake / pyflakes syntax checking

;;; Commentary:
;;; Python and other lanuage integration is determined by filename matching.
;; 

(require 'flymake)
(require 'flymake-python-pyflakes)

;;; Code:
(setq-default flymake-python-pyflakes-executable "pycheckers.py")

(defun netsight--flymake-pyflakes-init ()
  "Create a temporary buffer for extranal pychecker command."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list flymake-python-pyflakes-executable (list local-file))))

(setq flymake-allowed-file-name-masks
      (quote (("\\.py\\'" netsight--flymake-pyflakes-init)
              ("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init)
              ("\\.xml\\'" flymake-xml-init)
              ("\\.html?\\'" flymake-xml-init)
              ("\\.cs\\'" flymake-simple-make-init)
              ("\\.p[ml]\\'" flymake-perl-init)
              ("\\.php[345]?\\'" flymake-php-init)
              ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
              ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
              ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
              ("\\.tex\\'" flymake-simple-tex-init)
              ("\\.idl\\'" flymake-simple-make-init))))

(provide 'netsight-flymake)

;;; netsight-flymake.el ends here
