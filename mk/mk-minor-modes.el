;;; mk-minor-modes.el --- Minor modes' settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
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

(eval-when-compile
  (require 'flyspell)
  (require 'ido)
  (require 'multiple-cursors)
  (require 'smartparens))

(require 'cl-lib)
(require 'mk-utils)
(require 'smartparens-config)

(setq-default
 auto-fill-mode                    1       ; wrapping lines beyond limit
 auto-revert-verbose               nil     ; be quiet
 column-number-mode                t       ; display column number
 display-time-24hr-format          t       ; 24 hours format for time
 display-time-default-load-average nil     ; don't clutter my status line
 fill-column                       76      ; set fill column
 flycheck-emacs-lisp-initialize-packages t ; always initialize packages
 flycheck-emacs-lisp-load-path     'inherit
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
 show-paren-delay                  0.05
 sp-highlight-pair-overlay         nil
 sp-highlight-wrap-overlay         nil
 sp-highlight-wrap-tag-overlay     nil
 whitespace-line-column            80
 whitespace-style                  '(face lines-tail)
 yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))

(ace-popup-menu-mode              1) ; replace GUI popup menus
(blink-cursor-mode                0) ; my cursor doesn't blink, man
(delete-selection-mode            1) ; delete selection mode enabled
(display-time-mode                1) ; display time
(electric-indent-mode             0) ; I use aggressive indent mode instead
(global-aggressive-indent-mode    1) ; turn aggressive indent on globally
(global-auto-revert-mode          1) ; revert buffers automatically
(highlight-line-mode              1) ; highlight lines in list-like buffers
(ido-mode                         1) ; ido for `switch-buffer' and `find-file'
(ido-ubiquitous-mode              1) ; use IDO everywhere
(ido-vertical-mode                1) ; display IDO vertically
(menu-bar-mode                    0) ; hide menu bar
(minibuffer-electric-default-mode 1) ; electric minibuffer
(scroll-bar-mode                  0) ; disable scroll bar
(show-paren-mode                  1) ; highlight matching parenthesis
(smartparens-global-mode          1) ; smart editing of parenthesis
(tool-bar-mode                    0) ; hide tool bar
(vimish-fold-global-mode          1) ; persistent test folding
(whole-line-or-region-mode        1) ; operate on current line
(yas-global-mode                  1) ; enable Yasnippet

(dolist (buffer '("^\\*Backtrace\\*$"
                  "^\\*Compile-Log\\*$"
                  "^\\*.+Completions\\*$"
                  "^\\*Flycheck error messages\\*$"
                  "^\\*Help\\*$"
                  "^\\*Ibuffer\\*$"
                  "^\\*Messages\\*$"
                  "^\\*inferior-lisp\\*$"
                  "^\\*scratch\\*$"
                  "^\\*slime-compilation\\*$"
                  "^\\*slime-description\\*$"
                  "^\\*slime-events\\*$"))
  (add-to-list 'ido-ignore-buffers buffer))

(setq
 mk-minor-mode-alias
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

(defun flyspell-correct-previous (&optional words)
  "Correct word before point, reach distant words.

WORDS words at maximum are traversed backward until misspelled
word is found.  If it's not found, give up.  If argument WORDS is
not specified, traverse 12 words by default.

Return T if misspelled word is found and NIL otherwise.  Never
move point."
  (interactive "P")
  (let* ((Δ (- (point-max) (point)))
         (counter (string-to-number (or words "12")))
         (result
          (catch 'result
            (while (>= counter 0)
              (when (cl-some #'flyspell-overlay-p
                             (overlays-at (point)))
                (flyspell-correct-word-before-point)
                (throw 'result t))
              (backward-word 1)
              (setq counter (1- counter))
              nil))))
    (goto-char (- (point-max) Δ))
    result))

(τ flyspell flyspell "C-;" #'flyspell-correct-previous)

(τ smartparens smartparens "<C-backspace>" #'sp-backward-kill-sexp)
(τ smartparens smartparens "M-b"           #'sp-backward-sexp)
(τ smartparens smartparens "M-d"           #'sp-kill-sexp)
(τ smartparens smartparens "M-f"           #'sp-forward-sexp)
(τ smartparens smartparens "M-g"           #'sp-select-next-thing)
(τ smartparens smartparens "M-k"           #'sp-kill-hybrid-sexp)
(τ smartparens smartparens "M-t"           #'sp-add-to-previous-sexp)

(advice-add 'sp-add-to-previous-sexp :after (η #'just-one-space))
(advice-add 'sp-add-to-previous-sexp :after (η #'sp-forward-sexp))

(defun mk-prepare-text-mode ()
  "Enable some minor modes for text editing."
  (auto-fill-mode  1)
  (whitespace-mode 1)
  (flyspell-mode   1))

(defun mk-prepare-prog-mode ()
  "Enables some minor modes for programming."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode  1)
  (whitespace-mode 1)
  (flyspell-prog-mode)
  (flycheck-mode))

(add-hook 'after-change-major-mode-hook (ε #'mouse-wheel-mode 0))
(add-hook 'flycheck-mode-hook           #'flycheck-color-mode-line-mode)
(add-hook 'prog-mode-hook               #'hl-todo-mode)
(add-hook 'prog-mode-hook               #'mk-prepare-prog-mode)
(add-hook 'text-mode-hook               #'mk-prepare-text-mode)

(advice-add 'mc/prompt-for-inclusion-in-whitelist :override
            (lambda (_original-command) t))

(provide 'mk-minor-modes)

;;; mk-minor-modes.el ends here
