;;; mk-mu4e.el --- MU4E config -*- lexical-binding: t; -*-
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

;; MU4E configuration I use to manage emails from Emacs.  See the readme
;; file of this configuration for detailed instructions how to make this
;; work on Arch Linux.  In short, you will need to setup the following
;; external packages:
;;
;; * ‘offlineimap’
;; * ‘mu’
;; * ‘gnutls’

;;; Code:

(eval-when-compile
  (require 'smtpmail)
  (require 'starttls))

(require 'f)
(require 'mu4e nil t)

(setq
 mu4e-maildir                  (f-expand "~/Maildir")
 mu4e-drafts-folder            "/Drafts"
 mu4e-sent-folder              "/Sent"
 mu4e-trash-folder             "/Trash"
 mu4e-sent-messages-behavior   'sent
 mu4e-get-mail-command         "offlineimap" ; allows to use U to sync
 mu4e-completing-read-function #'ivy-completing-read
 mu4e-maildir-shortcuts
 '(("/Drafts" . ?d)
   ("/INBOX"  . ?i)
   ("/Sent"   . ?s)
   ("/Trash"  . ?t))
 mu4e-view-actions
 '(("in browser"       . mu4e-action-view-in-browser)
   ("capture message"  . mu4e-action-capture-message)
   ("show this thread" . mu4e-action-show-thread))
 message-send-mail-function    'smtpmail-send-it
 starttls-use-gnutls           t
 smtpmail-default-smtp-server  "smtp.openmailbox.org"
 smtpmail-smtp-server          "smtp.openmailbox.org"
 smtpmail-smtp-service         587
 smtpmail-debug-info           t)

(τ mu4e mu4e-headers "C--" #'ace-window)
(τ mu4e mu4e-view    "C--" #'ace-window)

(provide 'mk-mu4e)

;;; mk-mu4e.el ends here
