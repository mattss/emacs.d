;;; python-auto-magic.el --- Experimental stuff

;;; Commentary:
;;
;; autopep8 is very young and not quite production ready imho (mattr).
;;
;; If it ever gets to the state where it's reliably
;; stuff, the loading this automagic stuff might save a lot of time.
;; Replacing flymake with flycheck makes life soooo much better,
;; - responsiveness and reliability improved.
;; 
;; For now, if you want to experiment with this, you'll need to
;; to manually install the following package:
;;  `py-autopep8'
;; 
;; Add add:
;; (load-experiment "python-auto-magic.el")
;; to your `custom-file'
;;
;; The 'trick' to most of the functionality below, is in the
;; flycheck's post syntax check hook, we do something:
;; (unless (> 0 (length flycheck-current-errors))
;; which means that there were no syntax errors.
;; 
;; By default, `flycheck' will perform checking for each of the
;; events described here:
;;
;; http://flycheck.readthedocs.org/en/latest/manual/usage.html#el.variable.flycheck-check-syntax-automatically
;;
;; So if buffers are saved in `flycheck-after-syntax-check-hook'
;; then another syntax check will be triggered.
;; It's probably desirable to dynamically alter the vector:
;; `flycheck-check-syntax-automatically' (flycheck.readthedocs.org/en/latest/manual/api.html#el.variable.flycheck-current-errors)
;; to remove 'save from the list before calling (save-buffer)
;; then restoring it again afterwards in any `flycheck-after-syntax-check-hook'
;; when the default `flycheck-check-syntax-automatically' vector contains the
;; 'save symbol.
;; 
(require 'use-package)
(require 'flycheck)
(require 'py-isort)
(require 'vc)

;;; Code:

(defvar python-auto-magic--prev-vc-state-clean nil
  "Will be set to true if the vc state is clean/unregistered state.")

(defun python-auto-magic--vc-clean-or-unregistered? ()
  "Report t if vc state is clean"
  (when (eq major-mode 'python-mode)
    (let* ((pyfile (buffer-file-name (current-buffer)))
	   (vc-state-p (vc-state pyfile)))
      (or (eq vc-state-p 'up-to-date) (eq vc-state-p nil)))))
    

(defun python-auto-magic--pre-syntax-check ()
  "Run `py-isort' and `py-autopep8' over our source.

This is only done if the current `major-mode' is `python-mode'
and only if the version control state of the file
is clean or unregistred.

This is done *before* `flycheck' does syntax checks."
  (setq python-auto-magic--prev-vc-state-clean
	(python-auto-magic--vc-clean-or-unregistered?))
  (when python-auto-magic--prev-vc-state-clean
    (message "python-auto-magic--pre: Previous state was clean/unregistered")
    (message "python-auto-magic--pre: Running isort + autopep8")
    (py-isort)
    (py-autopep8)))
	      

(defun python-auto-magic--post-syntax-check ()
  "Peform conditional commit to VC.

The buffer *must* not have any local changes before the autopep8ing
and commiting will occur.

This feature will sort your imports and apply pep8.
It requires that you first install isort with pip.

$ pip install --user isort

This feature will commit changes locally immediately if successful,
using `vc-commit' - thus it will work for git, svn or any backend
supported by the `vc' package.

This feature does not currently pay attention to flymake errors,
although those could be factored in."
  (when (python-auto-magic--vc-clean-or-unregistered?)
    (when (> 0 (length flycheck-current-errors))
      (message "python-auto-magic--pos: flycheck found errors")
      (message "python-auto-magic--pre: aborting"))
    (unless (> 0 (length flycheck-current-errors))
      (let* ((pyfile (buffer-file-name (current-buffer)))
	     (vc-state-p (vc-state pyfile)))
	(unless (eq vc-state-p 'up-to-date)
	  (message "python-auto-magic--post: Running auto-commit")
	  (let* ((vc-backend-p (vc-backend pyfile))
		 (comment "emacs: isort + autopep8 changes only (flycheck)"))
	    (vc-checkin (list pyfile) vc-backend-p nil comment)))))))


(defun python-auto-magic-sort-imports-post-flycheck ()
  "Sort imports statements if `flycheck' succesful.

py-isort requires the external instalation of `isort':
$ pip install --user isort

Also benefits from editing of config file in ~/.isort
to tell it which names should be consdiered local etc.

See https://github.com/paetzke/py-isort.el for more info."
  (unless (> 0 (length flycheck-current-errors))
    (py-isort)))

;; Configure hooks in your `custom-file' to enable
;; specific hooks, e.g:
;;
;; (add-hook 'flycheck-before-syntax-check-hook
;; 	  'python-auto-magic--pre-syntax-check)

;; (add-hook 'flycheck-after-syntax-check-hook
;; 	  'python-auto-magic--post-syntax-check)


(provide 'python-auto-magic)
;;;  python-auto-magic.el ends here

