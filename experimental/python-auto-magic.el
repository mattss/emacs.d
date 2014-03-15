33(require 'py-autopep8)
(require 'py-isort)
(require 'vc)


(defun python-vc-dependant-auto-pep8 ()
  "Conditionally apply PEP8 to the current buffer and commit to VC.

The buffer *must* not have any local changes before the autopep8ing
and commiting will occur.

This feature will sort your imports and apply pep8.
It requires that you first install isort with pip.

$ pip install --user isort

This feature will commit changes locally immediately if successful,
using `vc-commit' - thus it will work for git, svn or any backend 
supported by the `vc' package. 

This feature does not currently pay attention to flymake errors,
although those could be factored in.
"
  (message "Running vc-dependant autopep8")
  (let* ((pyfile (buffer-file-name (current-buffer)))
        (vc-state-p (vc-state pyfile)))
    (when (or (equal vc-state-p 'up-to-date)
              (equal vc-state-p 'nil))
      (py-autopep8)
      (py-isort)
      (let ((flymake-error (flymake-goto-next-error)))
	(when flymake-error
	  (message "FLYMAKE ERROR!!!")))
      (save-buffer)
      (let* ((vc-state-p (vc-state pyfile))
             (syntax-errors nil))
        (unless (equal vc-state-p 'up-to-date)
          (let* ((vc-backend-p (vc-backend pyfile))
                (comment "emacs: isort + autopep8 changes only"))
            (vc-checkin (list pyfile) vc-backend-p nil comment))))))
  t)

(add-hook 'python-mode-hook 'python-vc-dependant-auto-pep8)

