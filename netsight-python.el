;;; netsight-python.el --- Customise python-mode

;; Copyright (C) 2014  mattr

;; Author: mattr <mattr@netsight.co.uk>
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
;; provides customisations of python-mode to enable features.
;; 

;;; Code:
(unless (require 'python-mode nil :noerr)
  (require 'python))
(require 'flymake-python-pyflakes)

(add-hook 'python-mode-hook 
          '(lambda ()
             (pep8-flymake-mode)))

(provide 'netsight-python)
;;; netsight-python.el ends here
