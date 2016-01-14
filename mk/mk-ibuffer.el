;;; mk-ibuffer.el --- iBuffer Settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–2016 Mark Karpov <markkarpov@openmailbox.org>
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

;; Various iBuffer-related settings.

;;; Code:

(eval-when-compile
  (require 'ibuf-ext)
  (require 'ibuffer))

(require 'mk-utils)

(setq
 ibuffer-expert t
 ibuffer-saved-filter-groups
 '(("default"
    ("Dired" (or (mode . dired-mode)
                 (mode . wdired-mode)))
    ("Emacs" (or (name . "^\\*scratch\\*$")
                 (name . "^\\*Messages\\*$")))
    ("ERC"   (mode     . erc-mode))
    ("Org"   (mode     . org-mode))
    ("Shell" (or (mode . eshell-mode)
                 (mode . shell-mode)))))
 ibuffer-show-empty-filter-groups nil)

(defun mk-ibuffer-setup ()
  "Execute some code to prepare iBuffer for operation."
  (ibuffer-auto-mode t)
  (ibuffer-switch-to-saved-filter-groups "default"))

(τ ibuffer ibuffer "<menu> ," (ε #'mk-first-line 3))
(τ ibuffer ibuffer "<menu> ." (ε #'mk-last-line  2))

(add-hook 'ibuffer-mode-hook #'mk-ibuffer-setup)

(provide 'mk-ibuffer)

;;; mk-ibuffer.el ends here
