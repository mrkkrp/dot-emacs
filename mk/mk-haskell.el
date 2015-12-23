;;; mk-haskell.el --- Haskell settings -*- lexical-binding: t; -*-
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

;; Everything I tweak for Haskell programming is here.

;;; Code:

(eval-when-compile
  (require 'ebal)
  (require 'haskell)
  (require 'smartparens))

(require 'cl-lib)
(require 'flycheck)
(require 'mk-utils)

(defvar mk-haskell-extensions
  '("Arrows"
    "AutoDeriveTypeable"
    "BangPatterns"
    "CPP"
    "DataKinds"
    "DeriveAnyClass"
    "DeriveDataTypeable"
    "DeriveFoldable"
    "DeriveFunctor"
    "DeriveGeneric"
    "DeriveTraversable"
    "EmptyDataDecls"
    "ExistentialQuantification"
    "ExplicitForAll"
    "FlexibleContexts"
    "FlexibleInstances"
    "ForeignFunctionInterface"
    "FunctionalDependencies"
    "GADTs"
    "GeneralizedNewtypeDeriving"
    "NoImplicitPrelude"
    "KindSignatures"
    "MagicHash"
    "MultiParamTypeClasses"
    "OverloadedLists"
    "OverloadedStrings"
    "QuasiQuotes"
    "Rank2Types"
    "RankNTypes"
    "RecordWildCards"
    "StandaloneDeriving"
    "TemplateHaskell"
    "TupleSections"
    "TypeFamilies"
    "TypeOperators"
    "ViewPatterns")
  "List of Haskell language extensions I use.")

(setq
 ebal-operation-mode                   'stack
 haskell-ask-also-kill-buffers         nil  ; don't ask
 haskell-process-load-or-reload-prompt t    ; please ask
 haskell-process-show-debug-tips       nil  ; don't show anything
 haskell-process-type                  'stack-ghci)

(add-to-list 'mk-search-prefix '(haskell-cabal-mode       . "haskell"))
(add-to-list 'mk-search-prefix '(haskell-interactive-mode . "haskell"))
(add-to-list 'mk-search-prefix '(haskell-mode             . "haskell"))
(add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode)

(defun mk-haskell-hoogle (symbol)
  "Find documentation for given symbol SYMBOL online."
  (interactive (list (mk-grab-input "Hoogle: ")))
  (browse-url
   (concat "https://www.stackage.org/lts/hoogle?q="
           (url-hexify-string symbol))))

(defun mk-haskell-package (symbol)
  "Find documentation for given package SYMBOL online."
  (interactive (list (mk-grab-input "Stackage: ")))
  (browse-url
   (concat "https://www.stackage.org/package/"
           (url-hexify-string symbol))))

(defun mk-haskell-set-min-versions (lib-list)
  "Help Flycheck handle Cabal MIN_VERSION_ definitions.

LIB-LIST should of the following form:

  (LIB-NAME V0 V1 V2)

Where LIB-NAME is a string, name of library and V0, V1, V2 are
version components."
  (dolist (item lib-list)
    (cl-destructuring-bind (lib a b c) item
      (let ((definition
              (format
               "((a<%d)||(a==%d&&b<%d)||(a==%d&&b==%d&&c<=%d))"
               a a b a b c)))
        (add-to-list
         'flycheck-ghc-args
         (format "-DMIN_VERSION_%s(a,b,c)=%s"
                 lib definition))
        (add-to-list
         'flycheck-hlint-args
         (format "--cpp-define=MIN_VERSION_%s(a,b,c)=%s"
                 lib definition))))))

(mk-haskell-set-min-versions
 '(("Cabal"   1 22 0)
   ("base"    4 8 0)
   ("process" 1 2 1)
   ("retry"   0 6 0)
   ("time"    1 5 0)))

(defun mk-haskell-add-pragma (pragma)
  "Add new language PRAGMA to current file and re-format pragmas."
  (interactive
   (list
    (completing-read "Extension: " mk-haskell-extensions)))
  (save-window-excursion
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (concat "^{-#[[:blank:]]+LANGUAGE[[:blank:]]+"
                   (regexp-quote pragma)
                   "[[:blank:]]+#-}")
           nil t 1)
          (message "Already there.")
        (let ((module-pos (re-search-forward "^module[[:blank:]]+" nil t 1)))
          (when module-pos
            (forward-line -1)
            (insert (concat "{-# LANGUAGE " pragma " #-}\n"))
            (goto-char (point-min))
            (re-search-forward
             "{-#\\(.\\|\n\\)*#-}"
             (+ module-pos 80) t 1)
            (let ((beg (match-beginning 0))
                  (end (match-end       0)))
              (sort-lines nil beg end)
              (align-regexp beg end "\\(\\s-*\\)#-}")
              (goto-char beg)
              (forward-line -1)
              (unless (looking-at "^$")
                (forward-line 1)
                (insert "\n")))))))))

(defun mk-haskell-add-import (import)
  "Add new IMPORT to current file and re-sort import declarations."
  (interactive (list (mk-grab-input "Import: " "import" t)))
  (save-window-excursion
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (concat "^" (regexp-quote import))
           nil t 1)
          (message "Already there.")
        (when (re-search-forward "^where\n+" nil t 1)
          (insert import "\n")
          (mark-paragraph)
          (sort-lines nil (region-beginning) (region-end)))))))

(τ haskell          haskell-interactive "C-c h" #'mk-haskell-hoogle)
(τ haskell          haskell-interactive "C-c n" #'mk-haskell-package)
(τ haskell          haskell-interactive "C-c r" #'haskell-process-restart)
(τ haskell          interactive-haskell "M-n"   #'mk-transpose-line-down)
(τ haskell          interactive-haskell "M-p"   #'mk-transpose-line-up)
(τ haskell-cabal    haskell-cabal       "C-c h" #'mk-haskell-hoogle)
(τ haskell-cabal    haskell-cabal       "C-c n" #'mk-haskell-package)
(τ haskell-cabal    haskell-cabal       "M-n"   #'mk-transpose-line-down)
(τ haskell-cabal    haskell-cabal       "M-p"   #'mk-transpose-line-up)
(τ haskell-commands haskell             "M-."   #'haskell-mode-jump-to-def)
(τ haskell-mode     haskell             "C-c h" #'mk-haskell-hoogle)
(τ haskell-mode     haskell             "C-c i" #'mk-haskell-add-import)
(τ haskell-mode     haskell             "C-c n" #'mk-haskell-package)
(τ haskell-mode     haskell             "C-c y" #'mk-haskell-add-pragma)
(τ haskell-mode     haskell             "M-,"   #'pop-tag-mark)

(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
(add-hook 'haskell-mode-hook  #'haskell-doc-mode)
(add-hook 'haskell-mode-hook  #'interactive-haskell-mode)

(provide 'mk-haskell)

;;; mk-haskell.el ends here
