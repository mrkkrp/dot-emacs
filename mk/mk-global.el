;;; mk-global.el --- Global settings -*- lexical-binding: t; -*-
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

;; Here are various settings not specific to any major mode. You can see
;; settings of various minor modes in `mk-minor-modes.el'.

;;; Code:

(require 'mk-abbrev)
(require 'mk-utils)

(setq-default
 auto-save-default                nil     ; don't ever create autosaves
 browse-url-browser-function      'browse-url-generic
 browse-url-generic-program       "icecat"
 compilation-read-command         nil
 compile-command                  "cd .. ; make -k"
 echo-keystrokes                  0.1     ; show keystrokes asap
 enable-recursive-minibuffers     t       ; use minibuffer recursively
 gc-cons-threshold                10240000 ; garbage collection every 10 Mb
 indent-tabs-mode                 nil     ; only spaces
 indicate-empty-lines             t       ; show where buffer's content ends
 inhibit-startup-screen           t       ; remove welcome screen
 initial-scratch-message          (concat ";; GNU Emacs " emacs-version "\n\n")
 kill-read-only-ok                t       ; don't rise errors, it's OK
 large-file-warning-threshold     10240000 ; warn when opening >10 Mb file
 major-mode                       'text-mode ; default mode is text mode
 make-backup-files                nil     ; don't create backups
 minibuffer-eldef-shorten-default t       ; shorten defaults in minibuffer
 require-final-newline            t
 resize-mini-windows              t       ; grow and shrink
 ring-bell-function               'ignore ; bells‽
 scroll-margin                    3
 scroll-step                      1
 suggest-key-bindings             nil
 tab-width                        4       ; tab width for text-mode
 user-full-name                   "Mark Karpov"
 user-mail-address                "markkarpov@openmailbox.org"
 safe-local-variable-values       '((Syntax  . ANSI-Common-Lisp)
                                    (Base    . 10)
                                    (Package . CL-USER)
                                    (Syntax  . COMMON-LISP)))

(put 'erase-buffer     'disabled nil) ; don't ever question my power
(put 'narrow-to-region 'disabled nil) ; ^

;;; Handy translations for use with «Sticky Keys»

(translate-kbd "<C-menu>"   "<menu>")
(translate-kbd "<C-return>" "<return>")
(translate-kbd "C-c c"      "C-c C-c")
(translate-kbd "C-c k"      "C-c C-k")
(translate-kbd "C-c l"      "C-c C-l")
(translate-kbd "C-c o"      "C-c C-o")
(translate-kbd "C-c v"      "C-c C-v")
(translate-kbd "C-x ;"      "C-x C-;")
(translate-kbd "C-x o"      "C-x C-o")

;;; Global key map

(π "C-'"        #'ace-window)
(π "C-c C-o"    #'find-file-at-point)
(π "C-c a"      #'org-agenda-list)
(π "C-c b"      #'compile-init-files)
(π "C-c e"      (ε #'visit-file mk-dir))
(π "C-c i"      #'flyspell-correct-word-before-point)
(π "C-c p"      #'purge-buffers)
(π "C-c r"      #'revert-buffer)
(π "C-c s"      #'mk-search)
(π "C-c t"      (ε #'visit-file (car org-agenda-files)))
(π "C-j"        #'newline)
(π "C-x o"      #'ace-window)
(π "M-c"        #'fix-word-capitalize)
(π "M-e"        #'mk-eval-last-sexp)
(π "M-j"        (ε #'delete-indentation t))
(π "M-l"        #'fix-word-downcase)
(π "M-n"        #'transpose-line-down)
(π "M-p"        #'transpose-line-up)
(π "M-r"        #'duplicate-line)
(π "M-u"        #'fix-word-upcase)
(π "M-x"        #'smex)
(π "<f2>"       #'save-buffer)
(π "<f5>"       #'find-file)
(π "<f6>"       #'find-file-other-window)
(π "<f7>"       (ε #'mk-use-lang "french-keyboard"  "fr"))
(π "<f8>"       (ε #'mk-use-lang "russian-computer" "ru"))
(π "<f9>"       (ε #'kill-buffer nil))
(π "<f10>"      #'delete-other-windows)
(π "<f11>"      #'switch-to-buffer)
(π "<f12>"      #'save-buffers-kill-terminal)
(π "<escape>"   #'delete-window)
(π "<return>"   #'avy-goto-char)
(π "<S-up>"     #'buf-move-up)
(π "<S-down>"   #'buf-move-down)
(π "<S-left>"   #'buf-move-left)
(π "<S-right>"  #'buf-move-right)
(π "<menu>"     nil)
(π "<menu> ,"   #'beginning-of-buffer)
(π "<menu> ."   #'end-of-buffer)
(π "<menu> /"   #'rectangle-mark-mode)
(π "<menu> 2"   #'mark-word)
(π "<menu> 3"   #'mark-rest-of-line)
(π "<menu> 5"   #'mark-paragraph)
(π "<menu> SPC" #'mk-abbrev-insert)
(π "<menu> a b" #'abbrev-mode)
(π "<menu> a p" #'apropos)
(π "<menu> a r" #'align-regexp)
(π "<menu> a s" #'write-file)
(π "<menu> b j" #'bookmark-jump)
(π "<menu> b k" #'bookmark-jump-other-window)
(π "<menu> b l" #'bookmark-bmenu-list)
(π "<menu> b s" #'bookmark-set)
(π "<menu> c a" #'calc)
(π "<menu> c c" #'copy-buffer)
(π "<menu> c i" #'cider-jack-in)
(π "<menu> c l" #'calendar)
(π "<menu> c r" #'copy-rectangle-as-kill)
(π "<menu> c s" #'set-buffer-file-coding-system)
(π "<menu> c w" #'count-words)
(π "<menu> d a" (ε #'show-date))
(π "<menu> d b" #'double-buffer)
(π "<menu> d c" #'describe-char)
(π "<menu> d d" #'show-default-dir)
(π "<menu> d i" #'diff)
(π "<menu> e b" #'erase-buffer)
(π "<menu> e e" #'eval-last-sexp)
(π "<menu> e r" #'erc)
(π "<menu> e v" #'eval-buffer)
(π "<menu> f f" #'find-function)
(π "<menu> f o" #'mk-set-font)
(π "<menu> f v" #'find-variable)
(π "<menu> g d" #'gdb)
(π "<menu> g l" #'goto-line)
(π "<menu> g n" #'gnus)
(π "<menu> g r" #'rgrep)
(π "<menu> h e" #'hexl-mode)
(π "<menu> h r" #'split-window-below)
(π "<menu> i r" #'indent-region)
(π "<menu> j a" #'mc/mark-all-like-this)
(π "<menu> j e" #'mc/edit-ends-of-lines)
(π "<menu> j i" #'mc/insert-numbers)
(π "<menu> j l" #'mc/edit-lines)
(π "<menu> j n" #'mc/mark-next-like-this)
(π "<menu> j p" #'mc/mark-previous-like-this)
(π "<menu> k n" #'insert-key-name)
(π "<menu> k r" #'kill-rectangle)
(π "<menu> l b" #'list-buffers)
(π "<menu> l i" #'slime)
(π "<menu> l p" #'list-packages)
(π "<menu> m a" #'magit-status)
(π "<menu> m d" #'markdown-mode)
(π "<menu> m n" #'man)
(π "<menu> n n" #'narrow-to-region)
(π "<menu> n w" #'widen)
(π "<menu> p a" #'package-autoremove)
(π "<menu> p i" #'package-install)
(π "<menu> p r" #'print-buffer)
(π "<menu> p u" #'package-upgrade-all)
(π "<menu> p y" #'run-python)
(π "<menu> q e" #'query-replace-regexp)
(π "<menu> q r" #'query-replace)
(π "<menu> r c" #'copy-to-register)
(π "<menu> r i" #'insert-register)
(π "<menu> r n" #'rectangle-number-lines)
(π "<menu> r r" #'reverse-region)
(π "<menu> s a" #'mark-whole-buffer)
(π "<menu> s c" #'run-scheme)
(π "<menu> s h" #'eshell)
(π "<menu> s l" #'sort-lines)
(π "<menu> s n" #'sort-numeric-fields)
(π "<menu> s r" #'string-rectangle)
(π "<menu> s s" (ε #'switch-to-buffer "*scratch*"))
(π "<menu> s t" (ε #'show-date t))
(π "<menu> t e" #'tetris)
(π "<menu> t h" #'mk-switch-theme)
(π "<menu> u t" (ε #'untabify (point-min) (point-max)))
(π "<menu> v e" #'version)
(π "<menu> v r" #'split-window-right)
(π "<menu> y a" #'yas-reload-all)
(π "<menu> y p" #'yank-primary)
(π "<menu> y r" #'yank-rectangle)

(defalias 'display-startup-echo-area-message (ε #'show-date))
(defalias 'list-buffers                      #'ibuffer)
(defalias 'yes-or-no-p                       #'y-or-n-p)

(add-hook 'after-change-major-mode-hook #'apply-mode-alias)
(add-hook 'before-save-hook             #'delete-trailing-whitespace)

(advice-add 'narrow-to-region           :after       (η #'keyboard-quit))
(advice-add 'package-install            :filter-args #'pkgi-filter-args)
(advice-add 'process-kill-buffer-query-function :override (σ t))
(advice-add 'revert-buffer              :filter-args (σ nil t))
(advice-add 'save-buffers-kill-terminal :filter-args (σ t))

(provide 'mk-global)

;;; mk-global.el ends here
