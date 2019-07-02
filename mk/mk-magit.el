;;; mk-magit.el --- Magit settings -*- lexical-binding: t; -*-
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

;; Magit mode settings.

;;; Code:

(eval-when-compile
  (require 'magit))

(require 'mk-utils)

(setq magit-clone-set-remote.pushDefault t)

(τ git-commit git-commit "M-n" #'mk-transpose-line-down)
(τ git-commit git-commit "M-p" #'mk-transpose-line-up)

(provide 'mk-magit)

;;; mk-magit.el ends here
