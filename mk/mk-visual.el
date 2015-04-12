;;; mk-visual.el --- Control appearance -*- lexical-binding: t; -*-
;;;
;;; Copyright © 2015 Mark Karpov <markkarpov@opmbx.org>
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; When run under X system, set color theme and switch to full screen mode.

;;; Code:

(when window-system
  (dolist (range '((#x2045 . #x2112)
                   (#x2127 . #x2201)
                   (#x2213 . #x2214)
                   (#x2207 . #x220e)
                   (#x2261 . #x2263)
                   (#x2266 . #xfffd)))
    (set-fontset-font t range "DejaVu Sans Mono"))
  (set-face-attribute 'default nil :family "Ubuntu Mono" :height 120)
  (set-face-attribute 'variable-pitch nil :family "Ubuntu Mono")
  (load-theme 'solarized-dark t)
  (toggle-frame-fullscreen))

(provide 'mk-visual)

;;; mk-visual.el ends here
