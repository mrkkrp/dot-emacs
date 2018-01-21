;;; mk-markdown.el --- Markdown settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–2018 Mark Karpov <markkarpov92@gmail.com>
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

;; Markdown mode settings.

;;; Code:

(eval-when-compile
  (require 'markdown-mode))

(require 'mk-utils)

(setq markdown-url-compose-char ?…)

(τ markdown-mode markdown "<return>" nil)
(τ markdown-mode markdown "C-c C-l" #'markdown-export)
(τ markdown-mode markdown "C-c C-v" #'markdown-preview)
(τ markdown-mode markdown "M-n"     #'mk-transpose-line-down)
(τ markdown-mode markdown "M-p"     #'mk-transpose-line-up)

(with-eval-after-load 'markdown-mode
  (set-face-font 'markdown-code-face "DejaVu Sans Mono"))

(provide 'mk-markdown)

;;; mk-markdown.el ends here
