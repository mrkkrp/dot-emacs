;;; -*- Mode: Emacs-Lisp; -*-
;;;
;;; .gnus, configuration file for GNUS
;;;
;;; Copyright (c) 2015 Mark Karpov
;;;
;;; The configuration file is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; The configuration file is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

(setq gnus-select-method '(nnimap "opmbx"
                                  (nnimap-address "imap.openmailbox.org")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl))
      user-mail-address "markkarpov@opmbx.org"
      user-full-names "Mark Karpov"
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.openmailbox.org" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.openmailbox.org" 587
                                   "markkarpov@openmailbox.com" nil))
      smtpmail-default-smtp-server "smtp.openmailbox.org"
      smtpmail-smtp-server "smtp.openmailbox.org"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
