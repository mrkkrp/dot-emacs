;;; mk-global.el --- Global settings -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–2016 Mark Karpov <markkarpov@openmailbox.org>
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

;; Here are various settings not specific to any major mode.  You can see
;; settings of various minor modes in `mk-minor-modes.el'.

;;; Code:

(eval-when-compile
  (require 'modalka)
  (require 'org))

(require 'cl-lib)
(require 'misc)
(require 'mk-haskell)
(require 'mk-python)
(require 'mk-utils)
(require 'xref)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set variables

(setq-default
 apropos-do-all                   t ; more extensive search
 auto-save-default                nil ; don't ever create autosaves
 avy-keys                         '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s) ; Dvorak
 avy-style                        'at-full ; can't use the default
 backup-by-copying                t
 backup-directory-alist
 (list (cons "." (f-expand "backups" user-emacs-directory)))
 blink-matching-delay             0.5
 blink-matching-paren             'jump-offscreen
 browse-url-browser-function      'browse-url-generic
 browse-url-generic-program       "firefox"
 char-menu
 '("—" "‘’" "“”" "…" "«»"
   ("Typography"
    "–" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
   ("Math"
    "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√" "∇")
   ("Arrows"
    "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
   ("Greek"
    "α" "β" "Δ" "δ" "ε" "ζ" "η" "θ" "λ" "μ" "ν" "ξ"
    "Ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω" "Ω")
   ("Diacritics"
    "́" "â" "à" "ô" "ê" "é" "è" "û" "ù" "ü" "î" "ï" "ç"))
 compilation-read-command         nil
 cursor-type                      '(bar . 1) ; thin vertical bar
 cursor-in-non-selected-windows   nil        ; don't show it there
 delete-old-versions              t ;  delete excess backups silently
 echo-keystrokes                  0.1 ; show keystrokes asap
 enable-recursive-minibuffers     t ; use minibuffer recursively
 indent-tabs-mode                 nil ; only spaces
 indicate-empty-lines             t ; show where buffer's content ends
 inhibit-startup-screen           t ; remove welcome screen
 initial-scratch-message          (concat ";; GNU Emacs " emacs-version "\n\n")
 gc-cons-threshold                2000000
 kept-new-versions                4
 kept-old-versions                2
 kill-read-only-ok                t ; don't rise errors, it's OK
 large-file-warning-threshold     10240000 ; warn when opening >10 Mb file
 major-mode                       'text-mode ; default mode is text mode
 make-backup-files                t ; yes, create them
 minibuffer-eldef-shorten-default t ; shorten defaults in minibuffer
 require-final-newline            t
 resize-mini-windows              t ; grow and shrink
 ring-bell-function               'ignore ; bells‽
 safe-local-variable-values
 '((Syntax  . ANSI-Common-Lisp)
   (Base    . 10)
   (Package . CL-USER)
   (Syntax  . COMMON-LISP))
 scroll-margin                    3
 scroll-step                      1
 suggest-key-bindings             nil
 tab-width                        4 ; tab width for text-mode
 user-full-name                   "Mark Karpov"
 user-mail-address                "markkarpov@openmailbox.org"
 vc-display-status                nil ; don't bloat mode line
 version-control                  t ; make numeric backups unconditionally
 x-underline-at-descent-line      t ; improve rendering (mode line)
 xref-after-jump-hook             (list #'recenter)
 xref-after-return-hook           nil)

(put 'erase-buffer     'disabled nil) ; don't ever question my power
(put 'narrow-to-region 'disabled nil) ; ^

(ace-link-setup-default) ; use ace-link in various major modes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handy translations for use with “Sticky Keys”

(mk-translate-kbd "<C-menu>"      "<menu>")
(mk-translate-kbd "<C-return>"    "<return>")
(mk-translate-kbd "<menu> <menu>" "M-x")
(mk-translate-kbd "C-c c"         "C-c C-c")
(mk-translate-kbd "C-c k"         "C-c C-k")
(mk-translate-kbd "C-c l"         "C-c C-l")
(mk-translate-kbd "C-c o"         "C-c C-o")
(mk-translate-kbd "C-c v"         "C-c C-v")
(mk-translate-kbd "C-x ;"         "C-x C-;")
(mk-translate-kbd "C-x o"         "C-x C-o")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate Russian input-method to compensate Dvorak in OS level

(fix-input "english-dvorak" "russian-computer" "mk-dvorak-russian")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key map

(π "C-,"        #'dabbrev-expand)
(π "C--"        #'ace-window)
(π "C-."        #'undo)
(π "C-SPC"      #'mk-mark-command)
(π "C-\\"       (ε #'mk-use-lang "mk-dvorak-russian" "ru"))
(π "C-c C-o"    #'find-file-at-point)
(π "C-c a"      #'org-agenda-list)
(π "C-c b"      #'mk-compile-init-files)
(π "C-c e"      (ε #'mk-visit-file mk-dir))
(π "C-c p"      #'kill-or-bury-alive-purge-buffers)
(π "C-c r"      #'revert-buffer)
(π "C-c s"      #'mk-search)
(π "C-c t"      (ε #'mk-visit-file (car org-agenda-files)))
(π "C-j"        #'newline)
(π "C-r"        #'mk-smart-indent)
(π "C-s"        #'swiper)
(π "C-z"        #'mk-copy-rest-of-line)
(π "M-H"        #'mark-paragraph)
(π "M-S"        #'mk-eat-indentation)
(π "M-c"        #'fix-word-capitalize)
(π "M-e"        #'mk-eval-last-sexp)
(π "M-h"        #'mark-word)
(π "M-j"        (ε #'delete-indentation t))
(π "M-l"        #'fix-word-downcase)
(π "M-n"        #'mk-transpose-line-down)
(π "M-o"        #'ace-link-org)
(π "M-p"        #'mk-transpose-line-up)
(π "M-r"        #'mk-duplicate-line)
(π "M-u"        #'fix-word-upcase)
(π "M-x"        #'counsel-M-x)
(π "M-z"        #'zzz-up-to-char)
(π "<f2>"       #'save-buffer)
(π "<f5>"       #'find-file)
(π "<f6>"       #'find-file-other-window)
(π "<f8>"       (ε #'mk-use-lang "mk-dvorak-russian" "ru"))
(π "<f9>"       #'kill-or-bury-alive)
(π "<f10>"      #'zygospore-toggle-delete-other-windows)
(π "<f11>"      #'switch-to-buffer)
(π "<f12>"      #'mk-exit-emacs)
(π "<escape>"   #'delete-window)
(π "<return>"   #'modalka-mode)
(π "<menu>"     nil)
(π "<menu> ,"   #'beginning-of-buffer)
(π "<menu> ."   #'end-of-buffer)
(π "<menu> - -" #'center-line)
(π "<menu> SPC" #'char-menu)
(π "<menu> a f" #'auto-fill-mode)
(π "<menu> a g" #'aggressive-indent-mode)
(π "<menu> a p" #'apropos)
(π "<menu> a r" #'align-regexp)
(π "<menu> a s" #'write-file)
(π "<menu> b l" #'bookmark-bmenu-list)
(π "<menu> b s" #'bookmark-set)
(π "<menu> c a" #'calc)
(π "<menu> c c" #'mk-copy-buffer)
(π "<menu> c g" #'customize-group)
(π "<menu> c i" #'cider-jack-in)
(π "<menu> c l" #'calendar)
(π "<menu> c r" #'copy-rectangle-as-kill)
(π "<menu> c s" #'set-buffer-file-coding-system)
(π "<menu> c w" #'count-words)
(π "<menu> d a" (ε #'mk-show-date))
(π "<menu> d b" #'mk-double-buffer)
(π "<menu> d c" #'describe-char)
(π "<menu> d d" #'mk-show-default-dir)
(π "<menu> d i" #'diff)
(π "<menu> e ;" #'eval-expression)
(π "<menu> e b" #'erase-buffer)
(π "<menu> e e" #'eval-last-sexp)
(π "<menu> e l" #'mk-add-to-end-of-lines)
(π "<menu> e r" #'erc)
(π "<menu> e v" #'eval-buffer)
(π "<menu> f f" #'find-function)
(π "<menu> f l" #'flycheck-list-errors)
(π "<menu> f n" #'mk-file-name-to-kill-ring)
(π "<menu> f o" #'mk-set-font)
(π "<menu> f v" #'find-variable)
(π "<menu> g d" #'gdb)
(π "<menu> g l" #'avy-goto-line)
(π "<menu> g n" #'gnus)
(π "<menu> g r" #'rgrep)
(π "<menu> h a" #'highlight-symbol-remove-all)
(π "<menu> h e" #'ebal-execute)
(π "<menu> h i" #'ebal-init)
(π "<menu> h n" #'highlight-symbol-next)
(π "<menu> h p" #'highlight-symbol-prev)
(π "<menu> h r" #'split-window-below)
(π "<menu> h s" #'highlight-symbol)
(π "<menu> h u" #'mk-bookmark-jump)
(π "<menu> i r" #'indent-region)
(π "<menu> j r" #'skewer-repl)
(π "<menu> j s" #'run-skewer)
(π "<menu> k r" #'kill-rectangle)
(π "<menu> l b" #'list-buffers)
(π "<menu> l i" #'slime)
(π "<menu> l l" #'list-processes)
(π "<menu> l p" #'list-packages)
(π "<menu> m a" #'magit-dispatch-popup)
(π "<menu> m c" #'magit-clone)
(π "<menu> m d" #'markdown-mode)
(π "<menu> m i" #'magit-init)
(π "<menu> m k" #'mk-make)
(π "<menu> m m" (ε #'switch-to-buffer "*Messages*"))
(π "<menu> m n" #'man)
(π "<menu> m s" #'magit-status)
(π "<menu> n n" #'mk-narrow-to-region)
(π "<menu> n w" #'widen)
(π "<menu> o"   #'mk-haskell-insert-symbol)
(π "<menu> p a" #'package-autoremove)
(π "<menu> p f" #'package-install-file)
(π "<menu> p i" #'package-install)
(π "<menu> p j" #'mk-python-run-dev-server)
(π "<menu> p r" #'print-buffer)
(π "<menu> p u" #'mk-package-upgrade-all)
(π "<menu> p y" #'run-python)
(π "<menu> q e" #'vr/query-replace)
(π "<menu> q r" #'query-replace)
(π "<menu> r b" #'report-emacs-bug)
(π "<menu> r c" #'copy-to-register)
(π "<menu> r e" #'revert-buffer)
(π "<menu> r i" #'insert-register)
(π "<menu> r n" #'rectangle-number-lines)
(π "<menu> r r" #'reverse-region)
(π "<menu> s a" #'mark-whole-buffer)
(π "<menu> s h" #'eshell)
(π "<menu> s l" #'sort-lines)
(π "<menu> s n" #'sort-numeric-fields)
(π "<menu> s p" #'speed-type-text)
(π "<menu> s r" #'string-rectangle)
(π "<menu> s s" (ε #'switch-to-buffer "*scratch*"))
(π "<menu> s t" (ε #'mk-show-date t))
(π "<menu> t h" #'mk-switch-theme)
(π "<menu> u h" (ε #'mk-bookmark-jump t))
(π "<menu> u t" (ε #'untabify (point-min) (point-max)))
(π "<menu> v a" #'vimish-fold-avy)
(π "<menu> v e" #'version)
(π "<menu> v f" #'vimish-fold)
(π "<menu> v r" #'split-window-right)
(π "<menu> v v" #'vimish-fold-delete)
(π "<menu> x i" #'mk-install)
(π "<menu> x u" #'mk-uninstall)
(π "<menu> y a" #'yas-reload-all)
(π "<menu> y p" #'mk-yank-primary)
(π "<menu> y r" #'yank-rectangle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modal editing

(modalka-define-kbd "SPC" "C-SPC")
;; '
(modalka-define-kbd "," "C-,")
;; -
(modalka-define-kbd "/" "M-.")
(modalka-define-kbd "." "C-.")
(modalka-define-kbd ":" "M-;")
(modalka-define-kbd ";" "C-;")
(modalka-define-kbd "?" "M-,")

(modalka-define-kbd "0" "C-0")
(modalka-define-kbd "1" "C-1")
(modalka-define-kbd "2" "C-2")
(modalka-define-kbd "3" "C-3")
(modalka-define-kbd "4" "C-4")
(modalka-define-kbd "5" "C-5")
(modalka-define-kbd "6" "C-6")
(modalka-define-kbd "7" "C-7")
(modalka-define-kbd "8" "C-8")
(modalka-define-kbd "9" "C-9")

(modalka-define-kbd "a" "C-a")
(modalka-define-kbd "b" "C-b")
(modalka-define-kbd "c c" "C-c C-c")
(modalka-define-kbd "c k" "C-c C-k")
(modalka-define-kbd "c v" "C-c C-v")
(modalka-define-kbd "d" "C-d")
(modalka-define-kbd "e" "C-e")
(modalka-define-kbd "f" "C-f")
(modalka-define-kbd "g" "C-g")
(modalka-define-kbd "h" "M-h")
(modalka-define-kbd "i" "C-i")
(modalka-define-kbd "j" "M-j")
(modalka-define-kbd "k" "C-k")
(modalka-define-kbd "l" "C-l")
(modalka-define-kbd "m" "C-m")
(modalka-define-kbd "n" "C-n")
(modalka-define-kbd "o" "C-o")
(modalka-define-kbd "p" "C-p")
(modalka-define-kbd "q" "M-q")
(modalka-define-kbd "r" "C-r")
(modalka-define-kbd "s" "C-s")
(modalka-define-kbd "t" "C-t")
(modalka-define-kbd "u" "C-u")
(modalka-define-kbd "v" "C-v")
(modalka-define-kbd "w" "C-w")
(modalka-define-kbd "x ;" "C-x C-;")
(modalka-define-kbd "x e" "C-x C-e")
(modalka-define-kbd "x o" "C-x C-o")
(modalka-define-kbd "y" "C-y")
(modalka-define-kbd "z" "M-z")

(modalka-define-kbd "A" "M-SPC")
(modalka-define-kbd "B" "M-b")
(modalka-define-kbd "C" "M-c")
(modalka-define-kbd "D" "M-d")
(modalka-define-kbd "E" "M-e")
(modalka-define-kbd "F" "M-f")
(modalka-define-kbd "G" "C-`")
(modalka-define-kbd "H" "M-H")
;; I
;; J
(modalka-define-kbd "K" "M-k")
(modalka-define-kbd "L" "M-l")
(modalka-define-kbd "M" "M-m")
(modalka-define-kbd "N" "M-n")
(modalka-define-kbd "O" "M-o")
(modalka-define-kbd "P" "M-p")
;; Q
(modalka-define-kbd "R" "M-r")
(modalka-define-kbd "S" "M-S")
(modalka-define-kbd "T" "M-t")
(modalka-define-kbd "U" "M-u")
(modalka-define-kbd "V" "M-v")
(modalka-define-kbd "W" "M-w")
;; X
(modalka-define-kbd "Y" "M-y")
(modalka-define-kbd "Z" "C-z")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other trickery

(defalias 'display-startup-echo-area-message (ε #'mk-show-date))
(defalias 'list-buffers #'ibuffer)
(defalias 'yes-or-no-p #'y-or-n-p)

(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook 'before-save-hook #'mk-single-empty-line)

(advice-add 'package-install :filter-args (lambda (args) (list (car args) t)))
(advice-add 'process-kill-buffer-query-function :override (σ t))
(advice-add 'revert-buffer :filter-args (σ nil t))

(provide 'mk-global)

;;; mk-global.el ends here
