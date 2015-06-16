;;; mk-lisp.el --- Commons Lisp settings -*- lexical-binding: t; -*-
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

;; Let's load SLIME and Slime Helper, if there is `slime-helper.el' file, we
;; byte-compile it and entire SLIME, and next time we will be able to load
;; SLIME faster.

;;; Code:

(require 'mk-utils)

(let* ((helper-el  (expand-file-name "~/quicklisp/slime-helper.el"))
       (helper-elc (byte-compile-dest-file helper-el)))
  (when (and (file-exists-p helper-el)
             (or (not (file-exists-p helper-elc))
                 (file-newer-than-file-p helper-el helper-elc)))
    (byte-compile-file helper-el t)
    (shell-command
     (concat "cd "
             (shell-quote-argument slime-path)
             " ; make compile contrib-compile")))
  (when (and (file-exists-p helper-elc)
             (not (find 'slime features)))
    (load-file helper-elc))
  (switch-back "*Compile-Log*")
  (switch-back "*Shell Command Output*"))

(setq inferior-lisp-program "sbcl") ; Steel Bank Common Lisp

(add-to-list 'major-mode-alias '(lisp-mode       . "λ"))
(add-to-list 'major-mode-alias '(slime-repl-mode . "iλ"))
(add-to-list 'minor-mode-alias '(slime-mode      . ""))
(add-to-list 'mk-search-prefix '(lisp-mode       . "common lisp"))
(add-to-list 'mk-search-prefix '(slime-repl-mode . "common lisp"))

(defun slime-in-package ()
  "Load specified package and switch to it."
  (interactive)
  (let ((pkg-name (read-string "Package name: ")))
    (slime-repl-eval-string
     (concat "(progn (asdf:load-system :"
             pkg-name
             ")(cl:in-package :"
             pkg-name
             "))"))))

(τ lisp-mode  lisp       "C-c h" #'slime-hyperspec-lookup)
(τ slime      slime      "M-n"   #'transpose-line-down)
(τ slime      slime      "M-p"   #'transpose-line-up)
(τ slime      slime-repl "C-c i" #'slime-in-package)
(τ slime      slime-repl "C-c r" #'slime-restart-inferior-lisp)
(τ slime-repl slime-repl "<f9>"  (ε #'slime-kill-all-buffers))

(add-hook 'slime-mode-hook #'rainbow-delimiters-mode)

(provide 'mk-lisp)

;;; mk-lisp.el ends here
