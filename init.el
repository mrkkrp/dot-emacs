;;; init.el --- Emacs configuration of Mark Karpov -*- lexical-binding: t; -*-
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

;; Here we install/load some packages, start Emacs server and load other
;; functionality.

;;; Code:

(let ((emacs-version-needed "25"))
  (when (version< emacs-version emacs-version-needed)
    (error "Emacs %s is way too old, install at least Emacs %s"
           emacs-version
           emacs-version-needed)))

;; Install packages from MELPA and ELPA.

(setq
 package-selected-packages
 '(ace-link             ; Quickly follow links
   ace-popup-menu       ; Replace GUI popup menus
   ace-window           ; Quickly switch windows
   aggressive-indent    ; Keep code always indented
   auctex               ; Integrated environment for *TeX*
   avy                  ; Move cursor effectively
   cider                ; Clojure IDE
   common-lisp-snippets ; Yasnippets for Common Lisp
   ebal                 ; Emacs interface to Cabal
   f                    ; Modern API for working with files and dirs
   fix-word             ; Convenient word transformation
   flycheck             ; On-the-fly syntax checking
   flycheck-color-mode-line ; Colorize mode line according to Flycheck status
   flycheck-haskell     ; Flycheck: Cabal projects and sandboxes
   ghc                  ; Improve Haskell REPL experience
   gitignore-mode       ; Major mode for editing .gitignore files
   haskell-mode         ; A Haskell editing mode
   highlight-line       ; Highlight lines in list-like buffers
   highlight-symbol     ; Automatic and manual symbol highlighting
   hl-todo              ; Highlight TODO and similar keywords
   ido-hacks            ; Put more IDO in your IDO
   ido-ubiquitous       ; Use IDO (nearly) everywhere
   ido-vertical-mode    ; Makes IDO-mode display vertically
   kill-or-bury-alive   ; Precise control over buffer killing in Emacs
   magit                ; A Git porcelain inside Emacs
   markdown-mode        ; Major mode for Markdown-formatted text files
   mk-abbrev            ; Peculiar way to use Emacs abbrevs
   modalka              ; Native modal editing of your own design
   multiple-cursors     ; Multiple cursors for Emacs
   org                  ; Outline-based template notes management
   rainbow-delimiters   ; Highlight brackets according to their depth
   rich-minority        ; Clean-up and beautify the list of minor-modes
   slime                ; Superior Lisp Interaction Mode for Emacs
   smart-mode-line      ; A powerful and beautiful mode-line for Emacs
   smartparens          ; Tricks for working with all kinds of parenthesis
   smex                 ; M-x interface with IDO-style fuzzy matching
   solarized-theme      ; The Solarized color theme
   vimish-fold          ; Fold text like in Vim
   visual-regexp        ; Regexp replace with interactive visual feedback
   whole-line-or-region ; Operate on current line if region undefined
   yaml-mode            ; Major mode for editing YAML serialization format
   yasnippet            ; Yet another snippet extension for Emacs
   ztree                ; Show directory structure as a tree
   zygospore            ; Reversible version of `delete-other-windows'
   zzz-to-char))        ; Fancy version of `zap-to-char' command

(require 'package)
(require 'bytecomp)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (when (and (assq package package-archive-contents)
             (not (package-installed-p package)))
    (package-install package t)))

;; Set up directories.

(require 'f)

(defvar mk-dir (f-expand "mk" user-emacs-directory)
  "This is directory where all the configuration files are kept.")

(add-to-list 'load-path mk-dir)

(setq custom-file (f-expand ".emacs-custom.el" user-emacs-directory))

;; Now we should be able to install directly from git repositories.

(require 'mk-utils)

(defvar package-selected-git-packages
  '((highlight-line . "https://github.com/mrkkrp/highlight-line.git")
    (mk-abbrev      . "https://github.com/mrkkrp/mk-abbrev.git"))
  "Alist of packages that are installed from git repositories.")

(dolist (package package-selected-git-packages)
  (unless (package-installed-p (car package))
    (package-install-git (cdr package))))

;; Start Emacs server.

(require 'server)

(unless (server-running-p)
  (server-start))

;; Require meat of the configuration.

(require 'mk-global)      ; global settings
(require 'mk-minor-modes) ; minor modes
(require 'mk-c)           ; major modes
(require 'mk-calendar)    ; ^
(require 'mk-clojure)     ; ^
(require 'mk-dired)       ; ^
(require 'mk-elisp)       ; ^
(require 'mk-erc)         ; ^
(require 'mk-eshell)      ; ^
(require 'mk-gnus)        ; ^
(require 'mk-haskell)     ; ^
(require 'mk-ibuffer)     ; ^
(require 'mk-lisp)        ; ^
(require 'mk-magit)       ; ^
(require 'mk-man)         ; ^
(require 'mk-markdown)    ; ^
(require 'mk-org)         ; ^
(require 'mk-prolog)      ; ^
(require 'mk-python)      ; ^
(require 'mk-tex)         ; ^
(require 'mk-texinfo)     ; ^
(require 'mk-visual)      ; control appearance

;;; init.el ends here
