;;; mk-haskell.el --- Haskell settings -*- lexical-binding: t; -*-
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

;; Everything I tweak for Haskell programming is here.  To generate tags, I
;; use ‘hasktags’ package.

;;; Code:

(eval-when-compile
  (require 'char-menu)
  (require 'haskell)
  (require 'smartparens))

(require 'cl-lib)
(require 'flycheck)
(require 'mk-utils)

(setq
 haskell-ask-also-kill-buffers         nil  ; don't ask
 haskell-process-load-or-reload-prompt t    ; please ask
 haskell-process-show-debug-tips       nil  ; don't show anything
 haskell-process-type                  'stack-ghci
 haskell-process-args-stack-ghci       '("--ghci-options=-ferror-spans"))

(with-eval-after-load 'smartparens
  (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-cabal-mode)
  (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode))

(defun mk-haskell-insert-symbol ()
  "Insert one of the Haskell symbols that are difficult to type."
  (interactive)
  (char-menu
   '("<-" "::"  "->"  "=>"  "="     ;; aoeui
     "<*" "<$>" "<*>" "<|>" "*>"))) ;; dhtns

(τ haskell          haskell-interactive "<end>"      nil)
(τ haskell          haskell-interactive "<escape>"   nil)
(τ haskell          haskell-interactive "<home>"     nil)
(τ haskell          haskell-interactive "<next>"     nil)
(τ haskell          haskell-interactive "<prior>"    nil)
(τ haskell          haskell-interactive "C-<prior>"  nil)
(τ haskell          haskell-interactive "C-c r" #'haskell-process-restart)
(τ haskell          interactive-haskell "M-n"   #'mk-transpose-line-down)
(τ haskell          interactive-haskell "M-p"   #'mk-transpose-line-up)
(τ haskell-cabal    haskell-cabal       "C-c h" #'mk-haskell-hoogle)
(τ haskell-cabal    haskell-cabal       "M-n"   #'mk-transpose-line-down)
(τ haskell-cabal    haskell-cabal       "M-p"   #'mk-transpose-line-up)
(τ haskell-commands haskell             "M-."   #'haskell-mode-jump-to-def)
(τ haskell-mode     haskell             "C-c C-u" #'haskell-mode-generate-tags)
(τ haskell-mode     haskell             "C-c y" #'hasky-extensions)
(τ haskell-mode     haskell             "M-,"   #'pop-tag-mark)

(defun mk-purge-the-fucking-thing ()
  "Purge the fucking thing."
  (define-key haskell-indentation-mode-map (kbd "RET") nil)
  (define-key haskell-indentation-mode-map (kbd "<backtab>") nil)
  (define-key haskell-indentation-mode-map (kbd ",") nil)
  (define-key haskell-indentation-mode-map (kbd ";") nil)
  (define-key haskell-indentation-mode-map (kbd ")") nil)
  (define-key haskell-indentation-mode-map (kbd "}") nil)
  (define-key haskell-indentation-mode-map (kbd "]") nil))

(add-hook 'haskell-indentation-mode-hook     #'mk-purge-the-fucking-thing)
(add-hook 'haskell-mode-hook                 #'interactive-haskell-mode)
(add-hook 'haskell-mode-hook                 #'mk-purge-the-fucking-thing)
(add-hook 'hasky-extensions-prettifying-hook #'mk-single-empty-line)
(add-hook 'hasky-extensions-prettifying-hook #'whitespace-cleanup)

(provide 'mk-haskell)

;;; mk-haskell.el ends here
