;;; mk-minor-modes.el --- Minor modes' settings -*- lexical-binding: t; -*-
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

;;; Settings of various minor modes, their activation, etc.

;;; Code:

(require 'mk-utils)

(setq-default
 auto-fill-mode                    1       ; wrapping lines beyond limit
 auto-revert-verbose               nil     ; be quiet
 column-number-mode                t       ; display column number
 display-time-24hr-format          t       ; 24 hours format for time
 display-time-default-load-average nil     ; don't clutter my status line
 fci-rule-column                   80      ; position of rule column
 fill-column                       76      ; set fill column
 global-auto-revert-non-file-buffers t     ; mainly for Dired
 ido-auto-merge-work-directories-length -1 ; disable it
 ido-create-new-buffer             'always
 ido-decorations          '("" "" "·" "…" "" "" " ×" " ✔" " ⊥" " ⊥" " ↯")
 ido-enable-flex-matching          t
 ido-everywhere                    t)

(blink-cursor-mode                 0) ; my cursor doesn't blink, man
(delete-selection-mode             1) ; delete selection mode enabled
(display-time-mode                 1) ; display time
(global-auto-revert-mode           1) ; revert buffers automatically
(global-subword-mode               1) ; move through camel case, etc.
(ido-mode                          1) ; ido for switch-buffer and find-file
(ido-ubiquitous-mode               1) ; use ido everywhere
(menu-bar-mode                     0) ; hide menu bar
(minibuffer-electric-default-mode  1) ; electric minibuffer
(scroll-bar-mode                   0) ; disable scroll bar
(show-paren-mode                   1) ; highlight parenthesis
(smooth-scroll-mode                1) ; smooth scroll
(tool-bar-mode                     0) ; hide tool bar
(which-function-mode               1) ; displays current function

(eval-after-load 'which-func
  '(setq which-func-format  (list (cadr which-func-format))
         which-func-unknown "⊥"))

(dolist (buffer '("^\*Backtrace\*"
                  "^\*Compile-Log\*"
                  "^\*.+Completions\*"
                  "^\*Flycheck error messages\*"
                  "^\*Help\*"
                  "^\*Ibuffer\*"
                  "^\*Messages\*"
                  "^\*inferior-lisp\*"
                  "^\*scratch\*"
                  "^\*slime-compilation\*"
                  "^\*slime-description\*"
                  "^\*slime-events\*"))
  (add-to-list 'ido-ignore-buffers buffer))

(put 'dired-do-copy    'ido      nil) ; use ido there
(put 'dired-do-rename  'ido      nil) ; ^
(put 'downcase-region  'disabled nil) ; don't ever doubt my power
(put 'erase-buffer     'disabled nil) ; ^
(put 'upcase-region    'disabled nil) ; ^

(setq
 minor-mode-alias
 '((abbrev-mode              . "") (inf-haskell-mode         . "")
   (auto-fill-function       . "") (interactive-haskell-mode . "")
   (eldoc-mode               . "") (ispell-minor-mode        . "")
   (flycheck-mode            . "") (magit-auto-revert-mode   . "")
   (flyspell-mode            . "") (slime-mode               . "")
   (haskell-doc-mode         . "") (smooth-scroll-mode       . "")
   (haskell-indent-mode      . "") (subword-mode             . "")
                                   (superword-mode           . "")))

(defun ido-key-bindings ()
  "Helper to define some non-standard key bindings in ido mode."
  (define-key ido-completion-map (kbd "C-b") #'ido-prev-match)
  (define-key ido-completion-map (kbd "C-f") #'ido-next-match))

(defun prepare-prog-mode ()
  "This function enables some minor modes for programming."
  (auto-fill-mode 1)
  (setq-local comment-auto-fill-only-comments t)
  (flyspell-prog-mode)
  (flycheck-mode))

(add-hook 'after-change-major-mode-hook #'abbrev-mode)
(add-hook 'after-change-major-mode-hook #'fci-mode)
(add-hook 'after-change-major-mode-hook (ε #'mouse-wheel-mode 0))
(add-hook 'flycheck-mode-hook           #'flycheck-haskell-setup)
(add-hook 'ibuffer-mode-hook            #'hl-line-mode)
(add-hook 'ido-setup-hook               #'ido-key-bindings)
(add-hook 'prog-mode-hook               #'prepare-prog-mode)
(add-hook 'text-mode-hook               #'auto-fill-mode)
(add-hook 'text-mode-hook               #'flyspell-mode)

(provide 'mk-minor-modes)

;;; mk-minor-modes.el ends here
