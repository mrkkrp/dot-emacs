;;; mk-lisp.el --- Commons Lisp settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–2017 Mark Karpov <markkarpov@openmailbox.org>
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

;; Let's load SLIME and Slime Helper, if there is ‘slime-helper.el’ file, we
;; byte-compile it and entire SLIME, and next time we will be able to load
;; SLIME faster.

;;; Code:

(eval-when-compile
  (require 'slime))

(require 'mk-utils)

(setq inferior-lisp-program "sbcl"
      slime-contribs '(slime-fancy))

(add-to-list 'mk-search-prefix '(lisp-mode       . "common lisp"))
(add-to-list 'mk-search-prefix '(slime-repl-mode . "common lisp"))

(kill-or-bury-alive-kill-with 'slime-repl-mode #'slime-kill-all-buffers t)

(defun mk-slime-in-package ()
  "Load specified package and switch to it."
  (interactive)
  (let ((pkg-name (read-string "Package name: ")))
    (slime-repl-eval-string
     (concat "(progn (asdf:load-system :"
             pkg-name
             ")(cl:in-package :"
             pkg-name
             "))"))))

(τ lisp-mode     lisp          "C-c h"    #'hyperspec-lookup)
(τ slime         slime         "M-n"      #'mk-transpose-line-down)
(τ slime         slime         "M-p"      #'mk-transpose-line-up)
;; (τ slime         slime-repl    "C-c i"    #'mk-slime-in-package)
;; (τ slime         slime-repl    "C-c r"    #'slime-restart-inferior-lisp)
(τ slime-autodoc slime-autodoc "SPC"      nil)
;; (τ slime-repl    slime-repl    "<return>" nil)

(eval-after-load 'slime ;; fucking SLIME, let me do my editing
  '(define-key slime-mode-indirect-map (kbd "SPC") nil))

(add-hook 'lisp-mode-hook       #'aggressive-indent-mode)
(add-hook 'slime-mode-hook      #'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook #'electric-indent-local-mode)

(provide 'mk-lisp)

;;; mk-lisp.el ends here
