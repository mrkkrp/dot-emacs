;;; mk-dired.el --- Dired settings -*- lexical-binding: t; -*-
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

;; Dired settings.

;;; Code:

(eval-when-compile
  (require 'dired-x)
  (require 'image-dired)
  (require 'wdired))

(require 'mk-utils)

(setq
 delete-by-moving-to-trash          t        ; in dired mode
 dired-auto-revert-buffer           t        ; automatically revert buffer
 dired-clean-up-buffers-too         t        ; kill buffers for deleted files
 dired-dwim-target                  t        ; guess target directory
 dired-keep-marker-copy             nil      ; don't mark copied files
 dired-listing-switches             "-GAlh --group-directories-first"
 dired-recursive-copies             'always  ; don't ask me, just do it
 dired-recursive-deletes            'always  ; ^
 image-dired-show-all-from-dir-max-files 127 ; a bit more
 wdired-allow-to-change-permissions t)       ; change permissions with Dired

(put 'dired-do-copy   'ido nil) ; use ido there
(put 'dired-do-rename 'ido nil) ; ^

(mk-disable-ido 'dired-create-directory)

(add-to-list 'major-mode-alias '(dired-mode                 . "δ"))
(add-to-list 'major-mode-alias '(image-dired-thumbnail-mode . "◊δ"))
(add-to-list 'major-mode-alias '(wdired-mode                . "↯δ"))

(defun dired-first-file ()
  "Jump to the first file in current directory."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 2))

(defun dired-last-file ()
  "Jump to the last file in current directory."
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(defun dired-open-external (file)
  "Open specified FILE with application determined by the OS."
  (interactive
   (list (dired-get-filename)))
  (call-process "xdg-open" nil 0 nil file))

(defun image-dired-show-current ()
  "Make preview and show all images in current directory."
  (interactive)
  (image-dired-show-all-from-dir dired-directory))

(τ dired dired   "<menu> ," #'dired-first-file)
(τ dired dired   "<menu> ." #'dired-last-file)
(τ dired dired   "b"        #'dired-up-directory)
(τ dired dired   "e"        #'dired-open-external)
(τ dired dired   "i"        #'image-dired-show-current)
(τ dired dired   "z"        #'wdired-change-to-wdired-mode)
(τ wdired wdired "<menu> ," #'dired-first-file)
(τ wdired wdired "<menu> ." #'dired-last-file)

(add-hook 'dired-mode-hook #'toggle-truncate-lines)

(advice-add 'wdired-change-to-dired-mode :after #'apply-mode-alias)

(provide 'mk-dired)

;;; mk-dired.el ends here
