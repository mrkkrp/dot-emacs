;;; mk-python.el --- Python mode configuration -*- lexical-binding: t; -*-
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

;; Some variables, key bindings, etc.  that I modify to code in Python.

;;; Code:

(eval-when-compile
  (require 'python))

(require 'mk-utils)

(setq-default
 python-fill-docstring-style 'django
 python-indent-offset        4)

(add-to-list 'mk-search-prefix '(inferior-python-mode . "python"))
(add-to-list 'mk-search-prefix '(python-mode          . "python"))

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

(defun mk-django-docs (symbol)
  "Find documentation for given symbol SYMBOL online."
  (interactive (list (mk-grab-input "Django Docs: ")))
  (browse-url
   (concat "https://docs.djangoproject.com/en/1.8/search/?q="
           (url-hexify-string symbol))))

(defun ipython-reset ()
  "Reset iPython shell."
  (interactive)
  (comint-send-string (python-shell-get-process)
                      "%reset\ny\n"))

(defun mk-python-run-dev-server ()
  "Run development server for current Django project."
  (interactive)
  (mk-with-directory-of-file "^manage.py$"
    (let ((compilation-buffer-name-function
           (lambda (_major-mode)
             (format "*%s-server*" (f-filename default-directory)))))
      (compile "python manage.py runserver" t)
      (browse-url "localhost:8000"))))

(defun mk-python-setup-django (shell &rest args)
  "Run Python shell via SHELL with arguments ARGS.

This sets DJANGO_SETTINGS_MODULE environment variable
automatically and calls the shell from appropriate directory
automatically."
  (let ((django-dir (mk-find-file "^manage.py$")))
    (if django-dir
        (let* ((default-directory django-dir)
               (manage (f-expand "manage.py"))
               (settings-module
                (with-temp-buffer
                  (insert-file-contents manage)
                  (re-search-forward
                   "os\\.environ\\.setdefault(\"DJANGO_SETTINGS_MODULE\", \"\\(.+\\)\")"
                   nil t)
                  (match-string-no-properties 1))))
          (setenv "DJANGO_SETTINGS_MODULE" settings-module)
          (apply shell args)
          (comint-send-string
           (python-shell-get-process)
           "import django\ndjango.setup()\n"))
      (apply shell args)
      (message "Started plain Python shell"))))

(τ python inferior-python "C-c h"   #'mk-python-docs)
(τ python inferior-python "C-c i"   #'mk-django-docs)
(τ python inferior-python "C-c r"   #'ipython-reset)
(τ python python          "C-c C-c" #'python-shell-send-defun)
(τ python python          "C-c C-l" #'python-shell-send-buffer)
(τ python python          "C-c h"   #'mk-python-docs)
(τ python python          "C-c i"   #'mk-django-docs)

(advice-add 'python-shell-send-buffer :before #'python-shell-ensure-proc)
(advice-add 'python-shell-send-defun  :before #'python-shell-ensure-proc)
(advice-add 'run-python               :after  (η #'python-shell-switch-to-shell))
(advice-add 'run-python               :around #'mk-python-setup-django)

(provide 'mk-python)

;;; mk-python.el ends here
