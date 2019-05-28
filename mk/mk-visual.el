;;; mk-visual.el --- Control appearance -*- lexical-binding: t; -*-
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

;; When run under X system, set color theme and switch to full screen mode.

;;; Code:

(eval-when-compile
  (require 'smart-mode-line))

(require 'mk-utils)

(when window-system
  (mk-set-font "DejaVu Sans Mono" 120)
  (load-theme 'zenburn t)
  (let ((sml/no-confirm-load-theme t))
    (sml/setup))
  (toggle-frame-fullscreen))

(provide 'mk-visual)

;;; mk-visual.el ends here
