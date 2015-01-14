;;; -*- Mode: Emacs-Lisp; -*-
;;;
;;; .emacs, configuration file for GNU Emacs 24.4.1
;;;
;;; Used packages (melpa recommended):
;;; * ace-window
;;; * auto-complete
;;; * cider
;;; * color-theme
;;; * fill-column-indicator
;;; * flycheck
;;; * haskell-mode
;;; * magit
;;; * prolog
;;; * rainbow-delimiters
;;; * smooth-scroll
;;; * solarized-theme
;;;
;;; Other packages:
;;; * aspell
;;;
;;; To use local version of Common Lisp Hyper Spec, download it and place in
;;; ~/.emacs.d/HyperSpec/
;;;
;;; Copyright (c) 2015 Mark Karpov
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                             Various Stuff                              ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-archives
      '(("gnu"       . "https://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa"     . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(require 'server)
(require 'smooth-scroll)

(load-file "~/quicklisp/slime-helper.elc") ; byte-compiled for speed

(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                               Variables                                ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default
 auto-fill-mode                    1       ; wrapping lines beyond limit
 auto-save-default                 nil     ; don't ever create autosaves
 browse-url-generic-program        "icecat" ; GNU IceCat
 browse-url-browser-function       'browse-url-generic ; use GNU IceCat
 column-number-mode                t       ; display column number
 common-lisp-hyperspec-root        "~/.emacs.d/HyperSpec/"
 delete-by-moving-to-trash         t       ; in dired mode
 display-time-24hr-format          t       ; 24 hours format for time
 erc-nick                          "mrkkrp"
 fci-rule-column                   80      ; position of rule column
 fill-column                       76      ; set fill column
 gnus-permanently-visible-groups   ""      ; always show all groups
 indent-tabs-mode                  nil     ; identation only with spaces
 inferior-lisp-program             "sbcl"  ; SBCL
 inhibit-startup-screen            t       ; remove welcome screen
 initial-scratch-message           ";; Lisp Interaction\n\n" ; scratch msg
 kill-read-only-ok                 t       ; don't rise errors, it's OK
 make-backup-files                 nil     ; don't create backups
 major-mode                        'text-mode ; default mode is text mode
 minibuffer-eldef-shorten-default  t       ; shorten defaults in minibuffer
 python-indent-guess-indent-offset nil     ; don't guess indent offset
 python-indent-offset              4       ; indent offset for python mode
 resize-mini-windows               t       ; grow and shrink
 ring-bell-function                'ignore ; no annoying alarms
 scroll-step                       1       ; convenient scrolling
 send-mail-function                'smtpmail-send-it
 smtpmail-smtp-server              "smtp.openmailbox.org"
 smtpmail-smtp-service             587
 tab-width                         4       ; tag width for text-mode
 user-full-name                    "Mark Karpov"
 user-mail-address                 "markkarpov@opmbx.org"
 safe-local-variable-values        '((Syntax  . ANSI-Common-Lisp)
                                     (Base    . 10)
                                     (Package . CL-USER)
                                     (Syntax  . COMMON-LISP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                 Modes                                  ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode             1)     ; delete selection mode enabled
(display-time-mode                 1)     ; displaying time
(menu-bar-mode                    -1)     ; hide menu bar
(minibuffer-electric-default-mode  1)     ; electric minibuffer
(put 'downcase-region 'disabled  nil)     ; don't ask anything when I use it
(put 'upcase-region   'disabled  nil)     ; see above
(scroll-bar-mode                  -1)     ; disable scroll bar
(show-paren-mode                   1)     ; highlight parenthesis
(tool-bar-mode                    -1)     ; hide tool bar
(smooth-scroll-mode                t)     ; smooth scroll
(which-function-mode               1)     ; displays current function

;; open .pl files as Prolog files, not Perl files
(add-to-list 'auto-mode-alist '("\\.\\(pl\\|pro\\|lgt\\)" . prolog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                Bindings                                ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *basic-buffers*
  '("*scratch*"
    "*Messages*"
    "irc.freenode.net:6667"
    "#lisp"
    "#emacs")
  "These are the buffers that I don't want to purge with
PURGE-BUFFERS command.")

(defun purge-buffers ()
  "Kill all buffer except those that have names listed in
*BASIC-BUFFERS*."
  (interactive)
  (dolist (x (remove-if (lambda (x)
                          (find (buffer-name x)
                                *basic-buffers*
                                :test #'string-equal))
                        (buffer-list)))
    (kill-buffer x))
  (switch-to-buffer (car *basic-buffers*))
  (delete-other-windows))

(defun save-buffer-with-cleanup (&optional arg)
  "Delete trailing whitespace and save current buffer. Argument
of this function is passed to SAVE-BUFFER."
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer arg))

(defun revert-buffer-without-talk ()
  "Revert current buffer without any confirmation."
  (interactive)
  (revert-buffer nil t))

(defun compile-c ()
  "Start compiling some C code. Don't ask anything. Makefile
should be in the parent directory of current directory. (Works
well for me because I always keep C sources in 'src'
subdirectory."
  (interactive)
  (compile "cd .. ; make -k"))

(global-set-key (kbd "C-x C-s") 'save-buffer-with-cleanup)
(global-set-key (kbd "C-c ,")   'beginning-of-buffer)
(global-set-key (kbd "C-c .")   'end-of-buffer)
(global-set-key (kbd "M-g")     'magit-status)
(global-set-key (kbd "C-x o")   'ace-window)
(global-set-key (kbd "C-c M-s") 'run-scheme)
(global-set-key (kbd "C-c M-j") 'cider-jack-in)
(global-set-key (kbd "C-c M-l") 'slime)
(global-set-key (kbd "C-c M-h") 'haskell-mode)
(global-set-key (kbd "C-c h")   'slime-hyperspec-lookup)
(global-set-key (kbd "C-c p")   'purge-buffers)
(global-set-key (kbd "C-c c")   'comment-region)
(global-set-key (kbd "C-c u")   'uncomment-region)
(global-set-key (kbd "C-c r")   'revert-buffer-without-talk)
(define-key slime-repl-mode-map (kbd "C-c r") 'slime-restart-inferior-lisp)
(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map (kbd "C-c C-l") 'compile-c)))
(eval-after-load "inf-haskell"
  '(progn
     (define-key inferior-haskell-mode-map (kbd "<tab>") 'dabbrev-expand)))
(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map (kbd "M-]") 'calendar-forward-month)
     (define-key calendar-mode-map (kbd "M-[") 'calendar-backward-month)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                 Hooks                                  ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *hidden-minor-modes*
  '(abbrev-mode
    auto-fill-function
    eldoc-mode
    flycheck-mode
    flyspell-mode
    inf-haskell-mode
    haskell-indent-mode
    haskell-doc-mode
    magit-auto-revert-mode
    smooth-scroll-mode)
  "Collection of minor modes that should not appear on the status
line.")

(defun purge-minor-modes ()
  "Puts empty strings for minor modes in *HIDDEN-MINOR-MODES*
into MINOR-MODE-ALIST, effectively evicting them from the status
line."
  (dolist (x *hidden-minor-modes*)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
        (setcar trg "")))))

(defun prepare-prog-mode ()
  "This function enables some minor modes when user works on some
source."
  (auto-fill-mode 1)
  (setq-local comment-auto-fill-only-comments t)
  (flyspell-prog-mode)
  (flycheck-mode))

(defun electric-indent-disable-locally ()
  "The name of the functions speaks for itself."
  (electric-indent-local-mode 0))

(add-hook 'after-change-major-mode-hook 'purge-minor-modes)
(add-hook 'text-mode-hook               'auto-fill-mode)
(add-hook 'text-mode-hook               'flyspell-mode)
(add-hook 'prog-mode-hook               'prepare-prog-mode)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(add-hook 'emacs-lisp-mode-hook         'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook             'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook            'rainbow-delimiters-mode)
(add-hook 'slime-mode-hook              'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook            'inf-haskell-mode)
(add-hook 'haskell-mode-hook            'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook            'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook            'electric-indent-disable-locally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                        Only Under Window System                        ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (set-face-attribute 'default
                      nil
                      :family "Inconsolata"
                      :height 120
                      :weight 'bold)
  (set-face-attribute 'variable-pitch
                      nil
                      :family "Inconsolata")
  (load-theme 'solarized-dark t)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                 Cider                                  ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq cider-show-error-buffer              nil
      cider-docview-fill-column            76
      cider-stacktrace-fill-column         76
      nrepl-buffer-name-show-port          nil
;     cider-repl-use-pretty-printing       t
      cider-repl-display-in-current-window t
      cider-repl-result-prefix             ";; => ")
