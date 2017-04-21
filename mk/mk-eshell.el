;;; mk-eshell.el --- Emacs shell settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–2017 Mark Karpov <markkarpov@openmailbox.org>
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs shell related customization.

;;; Code:

(require 'ace-window)
(require 'mk-utils)

(defun mk-eshell-other-window (fnc &optional arg)
  "Open Emacs shell (via FNC) in other window.

ARG is argument to pass to Emacs shell."
  (unless (cdr (window-list))
    (split-window-right))
  (aw-select " Ace - Shell"
             (lambda (window)
               (select-window window)
               (switch-to-buffer (funcall fnc arg)))))

(add-hook 'eshell-mode-hook #'compilation-shell-minor-mode)
(add-hook 'eshell-mode-hook #'smartparens-mode)

(advice-add 'eshell :around #'mk-eshell-other-window)

(τ em-basic eshell "C-c C-o" nil)

(provide 'mk-eshell)

;;; mk-eshell.el ends here
