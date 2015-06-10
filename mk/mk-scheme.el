;;; mk-scheme.el --- Scheme settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@opmbx.org>
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

;; Scheme mode settings. I don't really program Scheme, but one time I
;; wanted to. I would probably have added some stuff here if I were more
;; interested in programming Scheme.

;;; Code:

(require 'mk-utils)

(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)

(provide 'mk-scheme)

;;; mk-scheme.el ends here
