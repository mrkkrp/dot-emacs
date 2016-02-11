;;; mk-texinfo.el --- Texinfo settings -*- lexical-binding: t; -*-
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

;; Texinfo settings.  Currently I mainly translate Texinfo files into HTML
;; documents, this setup reflects the fact.

;;; Code:

(require 'mk-utils)

(defvar mk-texinfo-html-output-buffer-name "*texinfo-html-output*"
  "Name of buffer where HTML version of Texinfo document is written.")

(defun mk-texinfo-html-standalone (&optional output-buffer-name)
  "Generate standalone HTML version of currently open Texinfo document.

Output goes to OUTPUT-BUFFER-NAME, or, if it's not supplied, to
buffer named by `mk-texinfo-html-output-buffer-name'.  Return name
of the buffer."
  (let ((output-buffer-name (or output-buffer-name
                                mk-texinfo-html-output-buffer-name))
        (buffer-file-name (buffer-file-name))
        (css-file (car (f-glob "*.css" default-directory))))
    (if (not buffer-file-name)
        (error "Must be visiting a file")
      (save-window-excursion
        (shell-command
         (concat "texi2html -o - "
                 (when css-file
                   (concat "--css-include "
                           (shell-quote-argument css-file)
                           " "))
                 (shell-quote-argument (buffer-file-name)))
         output-buffer-name))
      output-buffer-name)))

(defun mk-texinfo-html-preview (&optional output-buffer-name)
  "Preview current Texinfo file as HTML document.

When OUTPUT-BUFFER-NAME is given, insert the output in the buffer
with that name."
  (interactive)
  (browse-url-of-buffer (mk-texinfo-html-standalone output-buffer-name)))

(defun mk-texinfo-html-export (&optional output-file)
  "Translate current Texinfo file into an HTML document.

Result file will be named as specified by OUTPUT-FILE argument.
If it's NIL, name of result file will be produced by replacing of
.tex extension with .html extension.  This command is smart
enough to find .css file if it's present in the default directory
and include it in the result document."
  (interactive
   (list (read-file-name "Export as: " default-directory)))
  (let* ((output-file (or output-file
                          (concat (file-name-sans-extension (buffer-file-name))
                                  ".html")))
         (init-buf (current-buffer))
         (init-point (point))
         (init-buf-string (buffer-string))
         (output-buffer (find-file-noselect output-file))
         (output-buffer-name (buffer-name output-buffer)))
    (mk-texinfo-html-standalone output-buffer-name)
    (with-current-buffer output-buffer
      (save-buffer))
    (when (buffer-modified-p init-buf)
      (erase-buffer)
      (insert init-buf-string)
      (save-buffer)
      (goto-char init-point))
    output-file))

(τ tex-info Texinfo "C-c C-l" #'mk-texinfo-html-export)
(τ tex-info Texinfo "C-c C-v" #'mk-texinfo-html-preview)

(provide 'mk-texinfo)

;;; mk-texinfo.el ends here
