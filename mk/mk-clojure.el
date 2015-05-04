;;; mk-clojure.el --- Clojure settings -*- lexical-binding: t; -*-
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

;;; Clojure settings.

;;; Code:

(require 'mk-utils)

(setq
 cider-docview-fill-column            fill-column
 cider-repl-display-in-current-window t
 cider-repl-result-prefix             ";; => "
 cider-show-error-buffer              nil
 cider-stacktrace-fill-column         fill-column
 nrepl-buffer-name-show-port          nil)

(defun clojure-docs ()
  "Find documentation for given symbol online."
  (interactive)
  (let ((input (if mark-active
                   (buffer-substring (region-beginning)
                                     (region-end))
                 (read-string "ClojureDocs: "))))
    (destructuring-bind (x &optional y)
        (split-string input "/")
      (browse-url
       (concat "http://clojuredocs.org/clojure."
               (if y x "core")
               (if (string= "" y) "" "/")
               (url-hexify-string (or y x)))))))

(τ cider-repl   cider-repl "<f9>"  #'cider-quit)
(τ cider-repl   cider-repl "C-c h" #'clojure-docs)
(τ clojure-mode clojure    "C-c h" #'clojure-docs)

(add-to-list 'major-mode-alias '(cider-repl-mode . "ic"))
(add-to-list 'major-mode-alias '(clojure-mode    . "c"))

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(provide 'mk-clojure)

;;; mk-clojure.el ends here
