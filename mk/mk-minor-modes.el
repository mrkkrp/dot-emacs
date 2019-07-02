;;; mk-minor-modes.el --- Minor modes' settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–present Mark Karpov <markkarpov92@gmail.com>
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
  (require 'avy)
  (require 'cyphejor)
  (require 'ivy)
  (require 'smartparens))

(require 'cl-lib)
(require 'flyspell-lazy)
(require 'flyspell)
(require 'mk-highlight-line)
(require 'mk-utils)
(require 'smartparens-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set variables

(setq-default
 ace-link-fallback-function        #'ace-link-org
 auto-fill-mode                    1       ; wrapping lines beyond limit
 auto-revert-verbose               nil     ; be quiet
 aw-background                     nil     ; do not make background gray
 aw-keys                           avy-keys ; see ‘mk-global’
 column-number-mode                t       ; display column number
 display-time-24hr-format          t       ; 24 hours format for time
 display-time-default-load-average nil     ; don't clutter my status line
 fill-column                       76      ; set fill column
 flycheck-emacs-lisp-initialize-packages t ; always initialize packages
 flycheck-emacs-lisp-load-path     'inherit
 flycheck-temp-prefix              ".flycheck"
 flyspell-lazy-disallow-buffers    nil     ; do spell checking everywhere
 flyspell-lazy-idle-seconds        1       ; a bit faster
 global-auto-revert-non-file-buffers t     ; mainly for Dired
 ispell-dictionary                 "en"    ; default dictionary
 ivy-use-selectable-prompt         t       ; make prompt selectable
 modalka-cursor-type               'box
 rm-whitelist                      "^↑$"
 rm-text-properties                '(("^↑$" 'face 'font-lock-doc-face))
 show-paren-delay                  0.05
 sp-highlight-pair-overlay         nil
 sp-highlight-wrap-overlay         nil
 sp-highlight-wrap-tag-overlay     nil
 whitespace-line-column            80
 whitespace-style                  '(face trailing tabs empty lines-tail)
 yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globally activate minor modes

(ace-popup-menu-mode              1) ; replace GUI popup menus
(blink-cursor-mode                0) ; my cursor doesn't blink, man
(cyphejor-mode                    1) ; shorten names of major modes
(delete-selection-mode            1) ; delete selection mode enabled
(display-time-mode                1) ; display time
(electric-indent-mode             0) ; I use aggressive indent mode instead
(global-auto-revert-mode          1) ; revert buffers automatically
(mk-highlight-line-mode           1) ; highlight lines in list-like buffers
(ivy-mode                         1) ; enable ivy completion
(menu-bar-mode                    0) ; hide menu bar
(minibuffer-electric-default-mode 1) ; electric minibuffer
(mouse-wheel-mode                 0) ; ignore mouse wheel
(rich-minority-mode               1) ; hide most minor modes lighters
(scroll-bar-mode                  0) ; disable scroll bar
(show-paren-mode                  1) ; highlight matching parenthesis
(smartparens-global-mode          1) ; smart editing of parenthesis
(tool-bar-mode                    0) ; hide tool bar
(whole-line-or-region-global-mode 1) ; operate on current line
(yas-global-mode                  1) ; enable Yasnippet


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various

(setq
 cyphejor-rules
 '(:upcase
   ("bookmark"    "→")
   ("buffer"      "β")
   ("diff"        "Δ")
   ("dired"       "δ")
   ("emacs"       "ε")
   ("eshell"      "εsh")
   ("inferior"    "i" :prefix)
   ("interaction" "i" :prefix)
   ("interactive" "i" :prefix)
   ("lisp"        "λ" :postfix)
   ("menu"        "▤" :postfix)
   ("mode"        "")
   ("package"     "↓")
   ("python"      "π")
   ("shell"       "sh" :postfix)
   ("text"        "ξ")
   ("wdired"      "↯δ")))

(dolist (buffer '("^\\*Backtrace\\*$"
                  "^\\*Compile-Log\\*$"
                  "^\\*.+Completions\\*$"
                  "^\\*Flycheck error messages\\*$"
                  "^\\*Help\\*$"
                  "^\\*Ibuffer\\*$"
                  "^\\*Messages\\*$"
                  "^\\*inferior-lisp\\*$"
                  "^\\*scratch\\*$"))
  (add-to-list 'ivy-ignore-buffers buffer))

(add-to-list 'ispell-extra-args "--sug-mode=ultra")

(defun mk-flyspell-correct-previous (&optional words)
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

(τ flyspell flyspell "C-," nil)
(τ flyspell flyspell "C-." nil)
(τ flyspell flyspell "C-;" #'mk-flyspell-correct-previous)

(τ smartparens smartparens "<C-backspace>" #'sp-backward-kill-sexp)
(τ smartparens smartparens "M-b"           #'sp-backward-sexp)
(τ smartparens smartparens "M-d"           #'sp-kill-sexp)
(τ smartparens smartparens "M-f"           #'sp-forward-sexp)
(τ smartparens smartparens "M-h"           #'sp-select-next-thing)
(τ smartparens smartparens "M-k"           #'sp-kill-hybrid-sexp)
(τ smartparens smartparens "M-t"           #'sp-add-to-previous-sexp)

(advice-add 'sp-add-to-previous-sexp :after (η #'just-one-space))
(advice-add 'sp-add-to-previous-sexp :after (η #'sp-forward-sexp))

(defun mk-prepare-text-mode ()
  "Enable some minor modes for text editing."
  (auto-fill-mode 1)
  (flyspell-lazy-mode 1)
  (flyspell-mode 1)
  (unless (string-equal (buffer-name) "COMMIT_EDITMSG")
    (modalka-mode 1))
  (whitespace-mode 1))

(defun mk-prepare-prog-mode ()
  "Enables some minor modes for programming."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1)
  (unless (eq major-mode 'haskell-mode)
    (flycheck-mode 1))
  (flyspell-lazy-mode 1)
  (flyspell-prog-mode)
  (hl-todo-mode 1)
  (modalka-mode 1)
  (whitespace-mode 1))

(add-hook 'compilation-mode-hook   #'modalka-mode)
(add-hook 'conf-toml-mode-hook     #'modalka-mode)
(add-hook 'conf-unix-mode-hook     #'modalka-mode)
(add-hook 'flycheck-mode-hook      #'flycheck-color-mode-line-mode)
(add-hook 'gitignore-mode-hook     #'mk-prepare-prog-mode)
(add-hook 'haskell-cabal-mode-hook #'mk-prepare-prog-mode)
(add-hook 'help-mode-hook          #'modalka-mode)
(add-hook 'info-mode-hook          #'modalka-mode)
(add-hook 'markdown-mode-hook      #'flycheck-mode)
(add-hook 'mustache-mode-hook      #'modalka-mode)
(add-hook 'prog-mode-hook          #'mk-prepare-prog-mode)
(add-hook 'proof-mode-hook         #'mk-prepare-prog-mode)
(add-hook 'text-mode-hook          #'mk-prepare-text-mode)
(add-hook 'yaml-mode-hook          #'mk-prepare-prog-mode)
(add-hook 'ztree-mode-hook         #'modalka-mode)

(provide 'mk-minor-modes)

;;; mk-minor-modes.el ends here
