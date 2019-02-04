;;; mk-proof-general.el --- Proof general settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2018–2019 Mark Karpov <markkarpov92@gmail.com>
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

;; Proof general settings.

;;; Code:

(require 'mk-utils)

(setq
 proof-splash-enable       nil ; what a stupid idea anyway
 proof-three-window-enable nil)

(τ proof-script proof "C-c C-s" #'proof-goto-point)
(τ proof-script proof "M-n"     #'mk-transpose-line-down)
(τ proof-script proof "M-p"     #'mk-transpose-line-up)

(provide 'mk-proof-general)

;;; mk-proof-general.el ends here
