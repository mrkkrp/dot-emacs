;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Top-level configuration file.

;;; Code:

(let ((emacs-version-needed "26"))
  (when (version< emacs-version emacs-version-needed)
    (error "Emacs %s is way too old, install at least Emacs %s"
           emacs-version
           emacs-version-needed)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install packages from MELPA

(setq
 package-selected-packages
 '(
   ace-link             ; Quickly follow links
   ace-popup-menu       ; Replace GUI popup menus
   ace-window           ; Quickly switch windows
   aggressive-indent    ; Keep code always indented
   auctex               ; Integrated environment for *TeX*
   avy                  ; Move cursor effectively
   avy-menu             ; Any-powered popup menu
   char-menu            ; Fast insertion of arbitrary symbols
   counsel              ; Various completion functions using Ivy
   cyphejor             ; Shorten names of major modes
   direnv               ; Direnv support
   dune                 ; Mode for the Dune build system
   f                    ; Modern API for working with files and dirs
   fix-input            ; Make input methods work with Dvorak
   fix-word             ; Convenient word transformation
   flycheck             ; On-the-fly syntax checking
   flycheck-color-mode-line ; Colorize mode line according to Flycheck status
   flycheck-mmark       ; Flycheck support for MMark markdown processor
   flyspell-lazy        ; Improve Flyspell responsiveness using idle timers
   git-link             ; Get GitHub URL for a buffer location
   gitignore-mode       ; Major mode for editing .gitignore files
   haskell-mode         ; A Haskell editing mode
   hasky-extensions     ; Toggle Haskell language extensions
   hasky-stack          ; Interface to the Stack Haskell development tool
   highlight-symbol     ; Automatic and manual symbol highlighting
   hl-todo              ; Highlight TODO and similar keywords
   ivy                  ; A generic completion mechanism
   js2-mode             ; Improved JavaScript editing mode
   kill-or-bury-alive   ; Precise control over buffer killing in Emacs
   magit                ; A Git porcelain inside Emacs
   markdown-mode        ; Major mode for Markdown-formatted text files
   merlin               ; Context sensitive completion for OCaml
   modalka              ; Native modal editing of your own design
   mustache-mode        ; Major mode for Mustache
   nix-mode             ; Major mode for editing Nix expressions
   proof-general        ; A generic front-end for proof assistants
   rainbow-delimiters   ; Highlight brackets according to their depth
   rich-minority        ; Clean-up and beautify the list of minor-modes
   rust-mode            ; A major mode for editing Rust source code
   smart-mode-line      ; A powerful and beautiful mode-line for Emacs
   smartparens          ; Tricks for working with all kinds of parenthesis
   swiper               ; Isearch with an overview
   terraform-mode       ; Major mode for Terraform
   tuareg               ; Major mode for OCaml
   typit                ; A cool typing game
   use-package          ; Declarative package configuration
   visual-regexp        ; Regexp replace with interactive visual feedback
   whole-line-or-region ; Operate on current line if region undefined
   yaml-mode            ; Major mode for editing YAML serialization format
   yasnippet            ; Yet another snippet extension for Emacs
   zenburn-theme        ; The Zenburn color theme
   ztree                ; Show directory structure as a tree
   zygospore            ; Reversible version of ‘delete-other-windows’
   zzz-to-char          ; Fancy version of ‘zap-to-char’ command

   ))

(require 'package)
(require 'bytecomp)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (when (and (assq package package-archive-contents)
             (not (package-installed-p package)))
    (package-install package t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up directories

(require 'f)

(defvar mk-dir (f-expand "mk" user-emacs-directory)
  "The directory where all the configuration files are kept.")

(add-to-list 'load-path mk-dir)

(setq custom-file (f-expand ".emacs-custom.el" user-emacs-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require the rest of the configuration

(require 'mk-global)
(require 'mk-packages)

;;; init.el ends here
