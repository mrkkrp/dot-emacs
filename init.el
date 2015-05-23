;;; init.el --- Emacs configuration of Mark Karpov -*- lexical-binding: t; -*-
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

;;; Here we install/load some packages, start Emacs server and load other
;;; functionality.

;;; Code:

(let ((emacs-version-needed "25"))
  (when (version< emacs-version emacs-version-needed)
    (error "Emacs %s is way too old, install at least Emacs %s"
           emacs-version
           emacs-version-needed)))

(defvar vital-packages
  '(ace-window            ; switching between windows
    auctex                ; for LaTeX
    buffer-move           ; move buffers easily
    cider                 ; Clojure development
    fill-column-indicator ; shows… fill column indicator
    flycheck              ; checking code on the fly
    flycheck-haskell      ; cabal sandboxes, etc.
    ghc                   ; improves Haskell REPL experience
    haskell-mode          ; Haskell development
    ido-hacks             ; various ido goodies
    ido-ubiquitous        ; use ido everywhere
    magit                 ; Emacs mode for git
    markdown-mode         ; markdown editing
    multiple-cursors      ; a cool feature…
    prolog                ; Prolog development
    rainbow-delimiters    ; highlight nested parenthesis (for Lisps)
    smooth-scroll         ; scrolling experience
    solarized-theme)      ; my favorite color theme
  "List of packages that are required for this setup.")

(require 'cl)
(require 'package)
(require 'bytecomp)

(add-to-list
 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(unless package-archive-contents ; Ensure all the packages are installed.
  (package-refresh-contents))    ; ^

(dolist (package vital-packages)        ; ^
  (unless (package-installed-p package) ; ^
    (package-install package)))         ; ^

(require 'smooth-scroll)
(require 'server)

(unless (server-running-p)
  (server-start))

(defvar mk-dir (expand-file-name "mk" user-emacs-directory))

(add-to-list 'load-path mk-dir)

(require 'mk-global)      ; global settings not specific to any mode
(require 'mk-minor-modes) ; settings of various minor modes
(require 'mk-abbrev)      ; some abbreviations to insert Unicode characters
(require 'mk-bookmark)    ; various major modes
(require 'mk-c)           ; ^
(require 'mk-calendar)    ; ^
(require 'mk-clojure)     ; ^
(require 'mk-diff)        ; ^
(require 'mk-dired)       ; ^
(require 'mk-elisp)       ; ^
(require 'mk-erc)         ; ^
(require 'mk-gnus)        ; ^
(require 'mk-haskell)     ; ^
(require 'mk-magit)       ; ^
(require 'mk-man)         ; ^
(require 'mk-markdown)    ; ^
(require 'mk-org)         ; ^
(require 'mk-prolog)      ; ^
(require 'mk-scheme)      ; ^
(require 'mk-slime)       ; ^
(require 'mk-tex)         ; ^
(require 'mk-visual)      ; control appearance

;;; init.el ends here
