;;; mk-haskell.el --- Haskell settings -*- lexical-binding: t; -*-
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

;; Everything I tweak for Haskell programming is here. Stuff here requires
;; `happy', `alex', and `ghc-mod' (with cabal).

;;; Code:

(require 'mk-utils)

(setq
 haskell-ask-also-kill-buffers   nil  ; don't ask
 haskell-process-show-debug-tips nil) ; don't show anything

(add-to-list 'aggressive-indent-excluded-modes 'haskell-interactive-mode)
(add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
(add-to-list 'major-mode-alias '(haskell-interactive-mode . "iH"))
(add-to-list 'major-mode-alias '(haskell-mode             . "H"))
(add-to-list 'minor-mode-alias '(haskell-doc-mode         . ""))
(add-to-list 'minor-mode-alias '(haskell-indent-mode      . ""))
(add-to-list 'minor-mode-alias '(inf-haskell-mode         . ""))
(add-to-list 'minor-mode-alias '(interactive-haskell-mode . ""))
(add-to-list 'mk-search-prefix '(haskell-interactive-mode . "haskell"))
(add-to-list 'mk-search-prefix '(haskell-mode             . "haskell"))

(defvar mk-cabal-operations
  '((build          . "cabal build")
    (check          . "cabal check")
    (clean          . "cabal clean")
    (configure      . "cabal configure --enable-tests --enable-benchmarks")
    (init           . "cabal init")
    (install        . "cabal update ; \
cabal install --only-dependencies --enable-tests --enable-benchmarks")
    (run            . "cabal run")
    (sandbox-delete . "cabal sandbox delete")
    (sandbox-init   . "cabal sandbox init")
    (test           . "cabal test --test-option=\"--maximum-test-size=50\""))
  "Collection of operations supported by `mk-cabal-action'.")

(defun mk-cabal-action (command)
  "Perform a Cabal command COMMAND.
COMMAND can be one of the operations listed in
`mk-cabal-operations'.  Completing read is used if the command is
called interactively."
  (interactive
   (list
    (intern
     (completing-read
      "Cabal operation: "
      (mapcar #'symbol-name
              (mapcar #'car mk-cabal-operations))
      nil
      t))))
  (let ((dir (mk-find-file "\\`.+\\.cabal\\'")))
    (if dir
        (compile
         (format "cd %s ; %s"
                 dir
                 (cdr (assoc command mk-cabal-operations))))
      (message "Please create ‘.cabal’ file for the project."))))

(τ haskell          haskell-interactive "C-c C-c" #'mk-cabal-action)
(τ haskell          haskell-interactive "C-c h"   #'haskell-hoogle)
(τ haskell          haskell-interactive "C-c r"   #'haskell-process-restart)
(τ haskell          interactive-haskell "C-c C-c" #'mk-cabal-action)
(τ haskell-cabal    haskell-cabal       "C-c C-c" #'mk-cabal-action)
(τ haskell-cabal    haskell-cabal       "C-c h"   #'haskell-hoogle)
(τ haskell-cabal    haskell-cabal       "M-n"     #'transpose-line-down)
(τ haskell-cabal    haskell-cabal       "M-p"     #'transpose-line-up)
(τ haskell-commands haskell             "M-."     #'haskell-mode-jump-to-def)
(τ haskell-mode     haskell             "C-c h"   #'haskell-hoogle)
(τ haskell-mode     haskell             "M-,"     #'pop-tag-mark)

(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
(add-hook 'haskell-mode-hook  #'interactive-haskell-mode)
(add-hook 'haskell-mode-hook  #'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook  #'turn-on-haskell-indent)

(advice-add 'haskell-session-new-assume-from-cabal :override (lambda ()))

(provide 'mk-haskell)

;;; mk-haskell.el ends here
