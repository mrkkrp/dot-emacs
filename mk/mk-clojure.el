;;; mk-clojure.el --- Clojure settings -*- lexical-binding: t; -*-
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

;;; Clojure settings.

;;; Code:

(eval-when-compile
  (require 'cider))

(require 'mk-utils)

(setq
 cider-docview-fill-column            fill-column
 cider-mode-line '(:eval (format " %s" (cider--modeline-info)))
 cider-repl-display-in-current-window t
 cider-repl-result-prefix             ";; => "
 cider-show-error-buffer              nil
 cider-stacktrace-fill-column         fill-column
 nrepl-buffer-name-show-port          nil)

(add-to-list 'mk-search-prefix '(cider-repl-mode . "clojure"))
(add-to-list 'mk-search-prefix '(clojure-mode    . "clojure"))

(kill-or-bury-alive-kill-with 'cider-repl-mode #'cider-quit t)

(defun mk-clojure-docs (symbol)
  "Find documentation for given symbol SYMBOL online."
  (interactive (list (mk-grab-input "Clojure Docs: " (mk-name-at-point))))
  (cl-destructuring-bind (x &optional y)
      (split-string symbol "/")
    (browse-url
     (concat "http://clojuredocs.org/clojure."
             (if y x "core")
             (if (string= "" y) "" "/")
             (url-hexify-string (or y x))))))

(τ cider-repl   cider-repl "C-c h" #'mk-clojure-docs)
(τ cider-repl   cider-repl "C-c r" #'cider-restart)
(τ clojure-mode clojure    "C-c h" #'mk-clojure-docs)

(add-hook 'cider-repl        #'electric-indent-local-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(provide 'mk-clojure)

;;; mk-clojure.el ends here
