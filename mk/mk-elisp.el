;;; mk-elisp.el --- Emacs Lisp settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
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

;; Some pretty things for Emacs Lisp coding.

;;; Code:

(eval-when-compile
  (require 'slime-autoloads))

(require 'mk-utils)

(setq eldoc-idle-delay 0.1) ; default is way too long

(add-to-list 'mk-major-mode-alias '(emacs-lisp-mode       . "ε"))
(add-to-list 'mk-major-mode-alias '(lisp-interaction-mode . "iε"))
(add-to-list 'mk-search-prefix    '(emacs-lisp-mode       . "emacs lisp"))

(τ lisp-mode emacs-lisp       "C-c h" #'hyperspec-lookup)
(τ lisp-mode lisp-interaction "C-c h" #'hyperspec-lookup)

(add-hook 'emacs-lisp-mode-hook       #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook       #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)

(provide 'mk-elisp)

;;; mk-elisp.el ends here
