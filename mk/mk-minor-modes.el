;;; mk-minor-modes.el --- Minor modes' settings -*- lexical-binding: t; -*-
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

;; Settings of various minor modes, their activation, etc.

;;; Code:

(require 'mk-utils)
(require 'smartparens-config)

(setq-default
 auto-fill-mode                    1       ; wrapping lines beyond limit
 auto-revert-verbose               nil     ; be quiet
 column-number-mode                t       ; display column number
 display-time-24hr-format          t       ; 24 hours format for time
 display-time-default-load-average nil     ; don't clutter my status line
 fill-column                       76      ; set fill column
 global-auto-revert-non-file-buffers t     ; mainly for Dired
 ido-auto-merge-work-directories-length -1 ; disable it
 ido-create-new-buffer             'always
 ido-decorations '("" "" "·" "…" "" "" " ×" " ✔" " ⊥" " ⊥" " ↯")
 ido-enable-flex-matching          t
 ido-everywhere                    t
 ido-vertical-decorations          '("\n→ " "" "\n  " "\n  …" "[" "]"
                                     " ×" " ✔" " ⊥" " ⊥" " ↯" "\n→ " "")
 ido-vertical-define-keys          'C-n-and-C-p-only
 ispell-dictionary                 "en"    ; default dictionary
 sp-highlight-pair-overlay         nil
 sp-highlight-wrap-overlay         nil
 sp-highlight-wrap-tag-overlay     nil
 whitespace-line-column            80
 whitespace-style                  '(face lines-tail)
 yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))

(blink-cursor-mode                0) ; my cursor doesn't blink, man
(delete-selection-mode            1) ; delete selection mode enabled
(display-time-mode                1) ; display time
(electric-indent-mode             0) ; I use aggressive indent mode instead
(global-aggressive-indent-mode    1) ; turn aggressive indent on globally
(global-auto-revert-mode          1) ; revert buffers automatically
(ido-mode                         1) ; ido for switch-buffer and find-file
(ido-ubiquitous-mode              1) ; use IDO everywhere
(ido-vertical-mode                1) ; display IDO vertically
(menu-bar-mode                    0) ; hide menu bar
(minibuffer-electric-default-mode 1) ; electric minibuffer
(scroll-bar-mode                  0) ; disable scroll bar
(show-paren-mode                  1) ; highlight matching parenthesis
(smartparens-global-mode          1) ; smart editing of parenthesis
(tool-bar-mode                    0) ; hide tool bar
(whole-line-or-region-mode        1) ; operate on current line
(yas-global-mode                  1) ; enable Yasnippet

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

(setq
 minor-mode-alias
 '((abbrev-mode                  . "")
   (aggressive-indent-mode       . "")
   (auto-fill-function           . "")
   (compilation-shell-minor-mode . "")
   (eldoc-mode                   . "")
   (flycheck-mode                . "")
   (flyspell-mode                . "")
   (ispell-minor-mode            . "")
   (magit-auto-revert-mode       . "")
   (smartparens-mode             . "")
   (smooth-scroll-mode           . "")
   (subword-mode                 . "")
   (superword-mode               . "")
   (whitespace-mode              . "")
   (whole-line-or-region-mode    . "")
   (yas-minor-mode               . "")))

(τ flyspell flyspell "C-;" #'flyspell-correct-word-before-point)

(τ smartparens smartparens "<C-backspace>" #'sp-backward-kill-sexp)
(τ smartparens smartparens "<menu> 2"      #'sp-select-next-thing)
(τ smartparens smartparens "<menu> 4"      #'sp-add-to-previous-sexp)
(τ smartparens smartparens "M-b"           #'sp-backward-sexp)
(τ smartparens smartparens "M-d"           #'sp-kill-sexp)
(τ smartparens smartparens "M-f"           #'sp-forward-sexp)
(τ smartparens smartparens "M-k"           #'sp-kill-hybrid-sexp)

(advice-add 'sp-add-to-previous-sexp :after #'sp-forward-sexp)

(eval-after-load 'multiple-cursors-core
  '(defun mc/prompt-for-inclusion-in-whitelist (original-command)
     "Always return T, regardless of ORIGINAL-COMMAND, just do it."
     t))

(defun prepare-text-mode ()
  "Enable some minor mode for plain text editing."
  (auto-fill-mode  1)
  (whitespace-mode 1)
  (flyspell-mode   1))

(defun prepare-prog-mode ()
  "Enables some minor modes for programming."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode  1)
  (whitespace-mode 1)
  (flyspell-prog-mode)
  (flycheck-mode))

(add-hook 'after-change-major-mode-hook (ε #'mouse-wheel-mode 0))
(add-hook 'ibuffer-mode-hook            #'hl-line-mode)
(add-hook 'prog-mode-hook               #'hl-todo-mode)
(add-hook 'prog-mode-hook               #'prepare-prog-mode)
(add-hook 'text-mode-hook               #'prepare-text-mode)

(provide 'mk-minor-modes)

;;; mk-minor-modes.el ends here
