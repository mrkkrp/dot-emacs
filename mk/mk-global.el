;;; mk-global.el --- Global settings -*- lexical-binding: t; -*-
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

;;; Here are various settings not specific to any major mode. You can see
;;; settings of various minor modes in `mk-minor-modes.el'.

;;; Code:

(require 'mk-utils)
(require 'mk-abbrev)

(setq-default
 auto-save-default                nil     ; don't ever create autosaves
 browse-url-browser-function      'browse-url-generic
 browse-url-generic-program       "icecat"
 echo-keystrokes                  0.1     ; show keystrokes asap
 gc-cons-threshold                10240000 ; garbage collection every 10 Mb
 indent-tabs-mode                 nil     ; only spaces
 indicate-empty-lines             t       ; show where buffer's content ends
 inhibit-startup-screen           t       ; remove welcome screen
 initial-scratch-message          ";; Μὴ μοῦ τοὺς κύκλους τάραττε\n\n"
 kill-read-only-ok                t       ; don't rise errors, it's OK
 large-file-warning-threshold     10240000 ; warn when opening >10 Mb file
 major-mode                       'text-mode ; default mode is text mode
 make-backup-files                nil     ; don't create backups
 minibuffer-eldef-shorten-default t       ; shorten defaults in minibuffer
 require-final-newline            t
 resize-mini-windows              t       ; grow and shrink
 ring-bell-function               'ignore ; bells?
 scroll-margin                    3
 scroll-step                      1
 suggest-key-bindings             nil
 tab-width                        4       ; tab width for text-mode
 user-full-name                   "Mark Karpov"
 user-mail-address                "markkarpov@opmbx.org"
 safe-local-variable-values       '((Syntax  . ANSI-Common-Lisp)
                                    (Base    . 10)
                                    (Package . CL-USER)
                                    (Syntax  . COMMON-LISP)))

(π "C-c r"      #'revert-buffer)
(π "C-c p"      #'purge-buffers)
(π "C-c s"      #'search-online)
(π "C-c g"      #'upgrade-all-packages)
(π "C-c b"      #'compile-init-files)
(π "C-c e"      (ε #'visit-file mk-dir))
(π "C-c t"      (ε #'visit-file (car org-agenda-files)))
(π "C-c a"      #'org-agenda-list)
(π "C-c i"      #'flyspell-correct-word-before-point)
(π "C-x o"      #'ace-window)
(π "C-'"        #'ace-window)
(π "C-j"        #'newline)
(π "M-p"        #'transpose-line-up)
(π "M-n"        #'transpose-line-down)
(π "<f2>"       #'save-buffer)
(π "<f5>"       #'find-file)
(π "<f6>"       #'find-file-other-window)
(π "<f7>"       (ε #'α "french-keyboard"  "fr"))
(π "<f8>"       (ε #'α "russian-computer" "ru"))
(π "<f9>"       (ε #'kill-buffer nil))
(π "<f10>"      #'delete-other-windows)
(π "<f11>"      #'switch-to-buffer)
(π "<f12>"      #'save-buffers-kill-terminal)
(π "<escape>"   #'delete-window)
(π "<C-return>" #'duplicate-line)
(π "<S-up>"     #'buf-move-up)
(π "<S-down>"   #'buf-move-down)
(π "<S-left>"   #'buf-move-left)
(π "<S-right>"  #'buf-move-right)
(π "<menu>"     nil)
(π "<menu> ,"   (ε #'push-mark))
(π "<menu> ."   (ε #'goto-char (mark)))
(π "<menu> /"   (ε #'goto-char (point-mid)))
(π "<menu> <"   (ε #'goto-char (point-min)))
(π "<menu> >"   (ε #'goto-char (point-max)))
(π "<menu> SPC" #'mk-abbrev-insert)
(π "<menu> a b" #'abbrev-mode)
(π "<menu> a p" #'apropos)
(π "<menu> a r" #'align-regexp)
(π "<menu> a s" #'write-file)
(π "<menu> c a" #'calc)
(π "<menu> c i" #'cider-jack-in)
(π "<menu> c l" #'calendar)
(π "<menu> c r" #'copy-rectangle-as-kill)
(π "<menu> c s" #'set-buffer-file-coding-system)
(π "<menu> d a" (ε #'show-date))
(π "<menu> d c" #'describe-char)
(π "<menu> d i" #'diff)
(π "<menu> e e" #'eval-last-sexp)
(π "<menu> e r" #'erc)
(π "<menu> e s" #'eshell)
(π "<menu> g d" #'gdb)
(π "<menu> g l" #'goto-line)
(π "<menu> g n" #'gnus)
(π "<menu> g r" #'rgrep)
(π "<menu> h r" #'split-window-below)
(π "<menu> k r" #'kill-rectangle)
(π "<menu> l b" #'list-buffers)
(π "<menu> l i" #'slime)
(π "<menu> l p" #'list-packages)
(π "<menu> m a" #'magit-status)
(π "<menu> m n" #'man)
(π "<menu> p r" #'print-buffer)
(π "<menu> q e" #'query-replace-regexp)
(π "<menu> q r" #'query-replace)
(π "<menu> r n" #'rectangle-number-lines)
(π "<menu> s c" #'run-scheme)
(π "<menu> s h" #'shell)
(π "<menu> s l" #'sort-lines)
(π "<menu> s r" #'string-rectangle)
(π "<menu> s s" (ε #'switch-to-buffer "*scratch*"))
(π "<menu> s t" (ε #'show-date t))
(π "<menu> t e" #'tetris)
(π "<menu> v e" #'version)
(π "<menu> v r" #'split-window-right)
(π "<menu> y r" #'yank-rectangle)

(defalias 'display-startup-echo-area-message (ε #'show-date))
(defalias 'list-buffers                      #'ibuffer)
(defalias 'yes-or-no-p                       #'y-or-n-p)

(add-hook 'after-change-major-mode-hook #'apply-mode-alias)
(add-hook 'before-save-hook             #'delete-trailing-whitespace)

(advice-add 'compile                    :filter-args (λ "cd .. ; make -k"))
(advice-add 'revert-buffer              :filter-args (λ nil t))
(advice-add 'save-buffers-kill-terminal :filter-args (λ t))

(provide 'mk-global)

;;; mk-global.el ends here
