;;; mk-python.el --- Python mode configuration -*- lexical-binding: t; -*-
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

;; Some variables, key bindings, etc. that I modify to code in Python.

;;; Code:

(eval-when-compile
  (require 'aggressive-indent)
  (require 'python))

(require 'mk-utils)

(setq-default python-indent-offset 4)

(add-to-list 'aggressive-indent-excluded-modes 'python-mode)

(add-to-list 'mk-major-mode-alias '(inferior-python-mode . "iπ"))
(add-to-list 'mk-major-mode-alias '(python-mode          . "π"))
(add-to-list 'mk-search-prefix    '(inferior-python-mode . "python"))
(add-to-list 'mk-search-prefix    '(python-mode          . "python"))

(when (executable-find "ipython")
  (setq
   python-shell-interpreter          "ipython"
   python-shell-prompt-output-regexp "Out\\[[0-9 +]\\]: "
   python-shell-prompt-regexp        "In \\[[0-9]+\\]: "))

(defun python-shell-ensure-proc (&rest _rest)
  "Make sure that python process is running for current buffer."
  (unless (python-shell-get-process)
    (let ((win (get-buffer-window)))
      (run-python nil nil t)
      (select-window win))))

(defun mk-python-docs (symbol)
  "Find documentation for given symbol SYMBOL online."
  (interactive (list (mk-grab-input "Python Docs: ")))
  (browse-url
   (concat "https://docs.python.org/3/search.html?q="
           (url-hexify-string symbol)
           "&check_keywords=yes&area=default")))

(defun ipython-reset ()
  "Reset iPython shell."
  (interactive)
  (comint-send-string (python-shell-get-process)
                      "%reset\ny\n"))

(τ python inferior-python "C-c h"   #'mk-python-docs)
(τ python inferior-python "C-c r"   #'ipython-reset)
(τ python python          "C-c C-c" #'python-shell-send-defun)
(τ python python          "C-c C-l" #'python-shell-send-buffer)
(τ python python          "C-c h"   #'mk-python-docs)

(advice-add 'python-shell-send-buffer :before #'python-shell-ensure-proc)
(advice-add 'python-shell-send-defun  :before #'python-shell-ensure-proc)
(advice-add 'run-python               :after  (η #'python-shell-switch-to-shell))

(provide 'mk-python)

;;; mk-python.el ends here
