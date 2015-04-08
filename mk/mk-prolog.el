;;; mk-prolog.el --- Prolog settings -*- lexical-binding: t; -*-
;;;
;;; Copyright © 2015 Mark Karpov <markkarpov@opmbx.org>
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Prolog mode settings.

;;; Code:

(require 'mk-utils)

;; open .pl files as Prolog files, not Perl files
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

(add-to-list 'major-mode-alias '(prolog-inferior-mode . "iP"))
(add-to-list 'major-mode-alias '(prolog-mode          . "P"))

(provide 'mk-prolog)

;;; mk-prolog.el ends here
