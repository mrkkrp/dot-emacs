;;; mk-dired.el --- Dired settings -*- lexical-binding: t; -*-
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

;; Dired settings.

;;; Code:

(require 'mk-utils)

(setq
 delete-by-moving-to-trash t        ; in dired mode
 dired-auto-revert-buffer  t        ; automatically revert buffer
 dired-dwim-target         t        ; guess target directory
 dired-keep-marker-copy    nil      ; don't mark copied files
 dired-listing-switches    "-GAlh --group-directories-first"
 dired-recursive-copies    'always  ; don't ask me, just do it
 dired-recursive-deletes   'always) ; ^

(τ dired dired "b" #'dired-up-directory)
(τ dired dired "z" #'wdired-change-to-wdired-mode)

(put 'dired-do-copy   'ido nil) ; use ido there
(put 'dired-do-rename 'ido nil) ; ^

(add-to-list 'major-mode-alias '(dired-mode  . "δ"))
(add-to-list 'major-mode-alias '(wdired-mode . "wδ"))

(add-hook 'dired-mode-hook #'hl-line-mode)

(advice-add 'wdired-change-to-dired-mode :after #'apply-mode-alias)

(provide 'mk-dired)

;;; mk-dired.el ends here
