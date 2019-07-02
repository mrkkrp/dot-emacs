;;; mk-org.el --- Org settings -*- lexical-binding: t; -*-
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

;; Org mode settings.

;;; Code:

(eval-when-compile
  (require 'org))

(require 'mk-utils)

(setq
 org-agenda-files
 (f-glob "*.org" (f-expand "org" user-emacs-directory))
 org-catch-invisible-edits 'show ; make point visible
 org-ellipsis              "…"   ; alternative ellipsis symbol
 org-hide-emphasis-markers t
 org-tags-column           54)

(τ org org "C-,"   #'dabbrev-expand)
(τ org org "C-c i" #'org-insert-link)

(advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
(advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
(advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
(advice-add 'org-todo           :after (η #'org-save-all-org-buffers))

(provide 'mk-org)

;;; mk-org.el ends here
