;;; mk-gnus.el --- GNUS settings -*- lexical-binding: t; -*-
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

;; GNUS settings.

;;; Code:

(require 'mk-utils)

(defvar gnus-saved-window-config nil
  "Saved window configuration that will be restored when you exit GNUS.")

(setq
 gnus-permanently-visible-groups   ""      ; always show all groups
 send-mail-function                'smtpmail-send-it
 gnus-select-method       '(nnimap "openmailbox"
                                   (nnimap-address "imap.openmailbox.org")
                                   (nnimap-server-port 993)
                                   (nnimap-stream ssl))
 message-send-mail-function        'smtpmail-send-it
 smtpmail-starttls-credentials     '(("smtp.openmailbox.org" 587 nil nil))
 smtpmail-auth-credentials         '(("smtp.openmailbox.org" 587
                                      "markkarpov@openmailbox.org" nil))
 smtpmail-default-smtp-server      "smtp.openmailbox.org"
 smtpmail-smtp-server              "smtp.openmailbox.org"
 smtpmail-smtp-service             587
 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(defun gnus-save-window-config (&rest _rest)
  "Save current window configuration in `gnus-saved-window-config'."
  (setq gnus-saved-window-config (current-window-configuration)))

(defun gnus-restore-window-config (&rest _rest)
  "Restore window configuration after exiting GNUS.
Configuration is supposed to be stored in
`gnus-saved-window-config'.  However, if it's NIL, nothing will
be understaken to restore the configuraiton."
  (when gnus-saved-window-config
    (set-window-configuration gnus-saved-window-config)))

(defun gnus-go-to-inbox (&rest _rest)
  "Go to line that has title \"INBOX\"."
  (search-forward "INBOX"))

(τ gnus       gnus-article "o"        #'ace-link-gnus)
(τ gnus-group gnus-group   "<menu> ." (ε #'mk-last-line 1))
(τ gnus-sum   gnus-summary "<menu> ." (ε #'mk-last-line 1))
(τ gnus-sum   gnus-summary "o"        #'ace-link-gnus)

(add-hook 'gnus-group-mode-hook   #'hl-line-mode)
(add-hook 'gnus-summary-mode-hook #'hl-line-mode)

(advice-add 'gnus            :after  #'gnus-go-to-inbox)
(advice-add 'gnus            :before #'gnus-save-window-config)
(advice-add 'gnus-group-exit :after  #'gnus-restore-window-config)

(provide 'mk-gnus)

;;; mk-gnus.el ends here
