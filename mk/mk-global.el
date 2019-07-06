;;; mk-global.el --- Global settings -*- lexical-binding: t; -*-
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

;; Here are various settings not specific to any major mode.  You can see
;; settings of various minor modes in ‘mk-minor-modes.el’.

;;; Code:

(eval-when-compile
  (require 'modalka)
  (require 'org))

(require 'cl-lib)
(require 'misc)
(require 'mk-haskell)
(require 'mk-nix)
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
 browse-url-generic-program       "google-chrome-stable"
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
    "Ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω" "Ω"))
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
 sentence-end-double-space        nil ; I prefer single space
 suggest-key-bindings             nil
 tab-width                        4 ; tab width for text-mode
 user-full-name                   "Mark Karpov"
 user-mail-address                "markkarpov92@gmail.com"
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

(mk-translate-kbd "<C-menu>"      "<next>")
(mk-translate-kbd "<C-return>"    "<return>")
(mk-translate-kbd "<next> <next>" "M-x")
(mk-translate-kbd "C-C s"         "C-c C-s")
(mk-translate-kbd "C-c b"         "C-c C-b")
(mk-translate-kbd "C-c c"         "C-c C-c")
(mk-translate-kbd "C-c d"         "C-c C-d")
(mk-translate-kbd "C-c k"         "C-c C-k")
(mk-translate-kbd "C-c l"         "C-c C-l")
(mk-translate-kbd "C-c n"         "C-c C-n")
(mk-translate-kbd "C-c o"         "C-c C-o")
(mk-translate-kbd "C-c u"         "C-c C-u")
(mk-translate-kbd "C-c v"         "C-c C-v")
(mk-translate-kbd "C-x ;"         "C-x C-;")
(mk-translate-kbd "C-x o"         "C-x C-o")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate Russian input-method to compensate Dvorak on OS level

(fix-input "english-dvorak" "russian-computer" "mk-dvorak-russian")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key map

(π "C-,"        #'dabbrev-expand)
(π "C-."        #'undo)
(π "C-SPC"      #'mk-mark-command)
(π "C-\\"       (ε #'mk-use-lang "mk-dvorak-russian" "ru"))
(π "C-c R"      #'revert-buffer-with-coding-system)
(π "C-c p"      #'kill-or-bury-alive-purge-buffers)
(π "C-c r"      #'revert-buffer)
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
(π "M-o"        #'ace-link)
(π "M-p"        #'mk-transpose-line-up)
(π "M-r"        #'mk-duplicate-line)
(π "M-u"        #'fix-word-upcase)
(π "M-x"        #'counsel-M-x)
(π "M-z"        #'zzz-up-to-char)
(π "<f12>"      #'mk-exit-emacs)
(π "<down>"     #'end-of-buffer)
(π "<up>"       #'beginning-of-buffer)
(π "<escape>"   #'kill-or-bury-alive)
(π "<return>"   #'modalka-mode)
(π "<home>"     #'find-file)
(π "<end>"      #'save-buffer)
(π "<prior>"    #'ace-window)
(π "C-<prior>"  #'switch-to-buffer)
(π "<next>"     nil)
(π "<next> - -" #'center-line)
(π "<next> DEL" #'char-menu)
(π "<next> a f" #'auto-fill-mode)
(π "<next> a g" #'aggressive-indent-mode)
(π "<next> a r" #'align-regexp)
(π "<next> a s" #'write-file)
(π "<next> b l" #'bookmark-bmenu-list)
(π "<next> b s" #'bookmark-set)
(π "<next> c a" #'calc)
(π "<next> c c" #'mk-copy-buffer)
(π "<next> c g" #'customize-group)
(π "<next> c i" #'mk-compile-init-files)
(π "<next> c l" #'calendar)
(π "<next> c r" #'copy-rectangle-as-kill)
(π "<next> c s" #'set-buffer-file-coding-system)
(π "<next> c w" #'count-words)
(π "<next> d a" (ε #'mk-show-date))
(π "<next> d b" #'mk-double-buffer)
(π "<next> d c" #'describe-char)
(π "<next> d d" #'mk-show-default-dir)
(π "<next> d f" #'delete-frame)
(π "<next> d i" #'diff)
(π "<next> e ;" #'eval-expression)
(π "<next> e b" #'erase-buffer)
(π "<next> e c" (ε #'mk-visit-file mk-dir))
(π "<next> e d" #'eval-defun)
(π "<next> e e" #'eval-last-sexp)
(π "<next> e l" #'mk-add-to-end-of-lines)
(π "<next> e v" #'eval-buffer)
(π "<next> f f" #'find-function)
(π "<next> f l" #'flycheck-list-errors)
(π "<next> f n" #'mk-file-name-to-kill-ring)
(π "<next> f o" #'mk-set-font)
(π "<next> f v" #'find-variable)
(π "<next> g g" #'git-link)
(π "<next> g r" #'rgrep)
(π "<next> h a" #'highlight-symbol-remove-all)
(π "<next> h n" #'highlight-symbol-next)
(π "<next> h p" #'highlight-symbol-prev)
(π "<next> h r" #'split-window-below)
(π "<next> h s" #'highlight-symbol)
(π "<next> h u" #'mk-bookmark-jump)
(π "<next> i r" #'indent-region)
(π "<next> k r" #'kill-rectangle)
(π "<next> l b" #'list-buffers)
(π "<next> l l" #'list-processes)
(π "<next> l p" #'list-packages)
(π "<next> m c" #'magit-clone)
(π "<next> m e" #'mk-melpa-page)
(π "<next> m d" #'markdown-mode)
(π "<next> m i" #'magit-init)
(π "<next> m m" (ε #'switch-to-buffer "*Messages*"))
(π "<next> m n" #'man)
(π "<next> m s" #'magit-status)
(π "<next> n f" #'make-frame)
(π "<next> n n" #'mk-narrow-to-region)
(π "<next> n o" #'mk-nixos-option)
(π "<next> n p" #'mk-nixos-package)
(π "<next> n w" #'widen)
(π "<next> o"   #'mk-haskell-insert-symbol)
(π "<next> p a" #'package-autoremove)
(π "<next> p f" #'package-install-file)
(π "<next> p i" #'package-install)
(π "<next> p p" #'mk-package-page)
(π "<next> p u" #'mk-package-upgrade-all)
(π "<next> p y" #'python-mode)
(π "<next> q e" #'vr/query-replace)
(π "<next> q r" #'query-replace)
(π "<next> r b" #'report-emacs-bug)
(π "<next> r c" #'copy-to-register)
(π "<next> r i" #'insert-register)
(π "<next> r n" #'rectangle-number-lines)
(π "<next> r r" #'reverse-region)
(π "<next> s a" #'mark-whole-buffer)
(π "<next> s h" #'eshell)
(π "<next> s l" #'sort-lines)
(π "<next> s n" #'sort-numeric-fields)
(π "<next> s r" #'string-rectangle)
(π "<next> s s" (ε #'switch-to-buffer "*scratch*"))
(π "<next> s t" (ε #'mk-show-date t))
(π "<next> t f" #'toggle-frame-fullscreen)
(π "<next> t h" #'mk-switch-theme)
(π "<next> t y" #'typit-advanced-test)
(π "<next> u h" (ε #'mk-bookmark-jump t))
(π "<next> u t" (ε #'untabify (point-min) (point-max)))
(π "<next> v e" #'version)
(π "<next> v r" #'split-window-right)
(π "<next> y a" #'yas-reload-all)
(π "<next> y p" #'mk-yank-primary)
(π "<next> y r" #'yank-rectangle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modal editing

(modalka-define-kbd "SPC" "C-SPC")
;; ' (handy as self-inserting symbol)
;; " (handy as self-inserting symbol)
(modalka-define-kbd "," "C-,")
;; - (handy as self-inserting symbol)
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
(modalka-define-kbd "c b" "C-c C-b")
(modalka-define-kbd "c c" "C-c C-c")
(modalka-define-kbd "c k" "C-c C-k")
(modalka-define-kbd "c n" "C-c C-n")
(modalka-define-kbd "c s" "C-c C-s")
(modalka-define-kbd "c u" "C-c C-u")
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
(define-key modalka-mode-map (kbd "I") #'zygospore-toggle-delete-other-windows)
(define-key modalka-mode-map (kbd "J") #'avy-goto-line)
(modalka-define-kbd "K" "M-k")
(modalka-define-kbd "L" "M-l")
(modalka-define-kbd "M" "M-m")
(modalka-define-kbd "N" "M-n")
(modalka-define-kbd "O" "M-o")
(modalka-define-kbd "P" "M-p")
(define-key modalka-mode-map (kbd "Q") #'mk-sort-lines-dwim)
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
