;;; mk-js.el --- JavaScript settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–2018 Mark Karpov <markkarpov92@gmail.com>
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

;; JavaScript Emacs settings.

;;; Code:

(require 'mk-utils)

(add-to-list 'mk-search-prefix '(js2-mode . "java script "))
(add-to-list 'auto-mode-alist  '("\\.js$" . js2-mode))

(defun mk-js-docs (symbol)
  "Find documentation for given SYMBOL online."
  (interactive (list (mk-grab-input "Java Script Docs: ")))
  (browse-url
   (concat "https://developer.mozilla.org/en-US/search?q="
           (url-hexify-string symbol)
           "&topic=js")))

(defun mk-jquery-docs (symbol)
  "Find documentation for given SYMBOL online."
  (interactive (list (mk-grab-input "jQuery Docs: ")))
  (browse-url
   (concat "https://api.jquery.com/?s="
           (url-hexify-string symbol))))

(τ js2-mode js2 "C-c h"   #'mk-js-docs)
(τ js2-mode js2 "C-c i"   #'mk-jquery-docs)
(τ js2-mode js2 "M-j"     (ε #'delete-indentation t))

(provide 'mk-js)

;;; mk-js.el ends here
