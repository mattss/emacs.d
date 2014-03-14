;;; netsight.el --- Netsight Emacs configuration
;;; Set the default PATH (yes inside emacs)
;;; This means that we can assume the same environment as the shell.
;;; TODO: Perhaps assert that certain ENV vars are present and exist.

;;; Commentary:
;;; Include sub-package which should be obvious of their function by
;;; naming.
;;; The sub-package `netsight-style' is an assortment of settings that 
;;; apply across many `minor-mode' and `major-mode's.
;; 
(require 'pungi)
(require 'netsight-utils)
(require 'netsight-dired)
(require 'netsight-rst)
(require 'netsight-keybindings)
(require 'netsight-flymake)
(require 'netsight-python)
(require 'netsight-style)

;;; Code:

;; Try to ensure that shell processes in emacs use the same $PATH
;; as the shell.
(setenv "PATH" "/usr/local/bin:/usr/bin:/sbin:/usr/sbin:/bin")
(let ((path (shell-command-to-string ". ~/.profile; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))


;; emacs server configuration
;; Allows use with screen
;; Start either gnuserv or emacsserver for external access
(setq-default server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(setq-default server-use-tcp 't)
(when (not (or (eq window-system nil) (eq 'windows-nt system-type))) (server-start))

(provide 'netsight)

;;; netsight.el ends here
