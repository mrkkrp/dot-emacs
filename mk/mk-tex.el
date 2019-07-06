;;; mk-tex.el --- Tex and LaTex settings -*- lexical-binding: t; -*-
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

;; Some tweaks of Tex/LaTeX mode.  I mainly use LaTeX to create good-looking
;; PDF documents, so my setup is quite narrowly targeted.

;;; Code:

(require 'mk-utils)

(defun mk-pdf-latex-generate (&optional output-dir)
  "Generate PDF document from currently opened LaTeX document.

If given, OUTPUT-DIR specifies directory where all the temporary
and result files a are stored.  If it's not given, new directory
with unique name is used instead.  Return name of PDF file
produced."
  (let* ((output-dir (or output-dir (make-temp-file "pdflatex" t)))
         (buffer-file-name (buffer-file-name))
         (output-pdf (f-join output-dir
                             (f-swap-ext (f-base buffer-file-name)
                                         "pdf"))))
    (if (not buffer-file-name)
        (error "Must be visiting a file")
      (save-window-excursion
        (shell-command
         (concat "pdflatex -output-directory "
                 (shell-quote-argument output-dir)
                 " "
                 (shell-quote-argument buffer-file-name))))
      (message "Written %s" output-pdf)
      output-pdf)))

(defun mk-pdf-latex-preview ()
  "Generate temporary PDF file and open it with external application."
  (interactive)
  (call-process "xdg-open" nil 0 nil (mk-pdf-latex-generate)))

(defun mk-pdf-latex-export ()
  "Create PDF document based on current TeX/LaTeX file."
  (interactive)
  (mk-pdf-latex-generate default-directory))

(τ latex LaTeX "C-c C-l" #'mk-pdf-latex-export)
(τ latex LaTeX "C-c C-v" #'mk-pdf-latex-preview)

(advice-add 'TeX-insert-quote :filter-args (σ t))

(provide 'mk-tex)

;;; mk-tex.el ends here
