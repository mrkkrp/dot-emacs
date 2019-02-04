;;; mk-highlight-line.el --- Highlight lines in list-like buffers -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–2019 Mark Karpov <markkarpov92@gmail.com>
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

;; Emacs has that pretty minor mode for highlighting current line:
;; ‘hl-line-mode’, not many people use it all the time, but it's worth
;; enabling in many types of buffers that deal with lists of items, one item
;; per line.  This packages allows to enable it everywhere for readability.

;;; Code:

(defvar mk-highlight-line-target-modes
  '(Buffer-menu-mode
    bookmark-bmenu-mode
    dired-mode
    gnus-group-mode
    gnus-summary-mode
    ibuffer-mode
    package-menu-mode)
  "List of modes that are affected by ‘mk-highlight-line-mode’.")

;;;###autoload
(define-minor-mode mk-highlight-line-mode
  "Toggle highlight-line-mode minor mode.

With a prefix argument ARG, enable highlight-line if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
‘toggle’.

This minor mode is global. When it's active, major modes listed
in ‘mk-highlight-line-target-modes’ activate ‘hl-line-mode’
automatically."
  :global t
  (dolist (mode mk-highlight-line-target-modes)
    (funcall
     (if mk-highlight-line-mode
         #'add-hook
       #'remove-hook)
     (intern (concat (symbol-name mode) "-hook"))
     #'hl-line-mode)))

(provide 'mk-highlight-line)

;;; mk-highlight-line.el ends here
