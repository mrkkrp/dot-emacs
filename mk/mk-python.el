;;; mk-python.el --- Python mode configuration -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–present Mark Karpov <markkarpov92@gmail.com>
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

;; Some variables, key bindings, etc.  that I modify to code in Python.

;;; Code:

(eval-when-compile
  (require 'python))

(require 'mk-utils)

(setq-default
 python-fill-docstring-style 'pep-257-nn
 python-indent-offset        2)

(add-to-list 'auto-mode-alist '("\\.bzl$" . python-mode))
(add-to-list 'auto-mode-alist '("BUILD$" . python-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE$" . python-mode))

(provide 'mk-python)

;;; mk-python.el ends here
