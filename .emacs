;;; .emacs --- Emacs configuration file (GNU Emacs 24.4.1)
;;;
;;; Commentary:
;;;
;;; Used packages (automatically installed if needed):
;;; * ace-window
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
;;; Other packages (OS level):
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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                             Various Stuff                              ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list
 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(defvar vital-packages
  '(ace-window
    cider
    color-theme
    fill-column-indicator
    flycheck
    haskell-mode
    magit
    prolog
    rainbow-delimiters
    smooth-scroll
    solarized-theme)
  "List of packages that must be installed.")

;; install all the packages automatically if they are not installed
(dolist (package vital-packages)
  (unless (package-installed-p package)
    (package-install package)))

(eval-when-compile (require 'cl)) ; add Common Lisp functions
(require 'server)
(require 'smooth-scroll)
(require 'bytecomp)

;; Let's load SLIME with Slime Helper, if there is 'slime-helper.el' file,
;; we byte-compile it and entire SLIME, and next time we will be able to
;; load SLIME faster. If there is more recent version of 'slime-helper.el'
;; available, we should recompile it (and SLIME).
(defvar slime-helper-el "~/quicklisp/slime-helper.el")
(defvar slime-helper-elc (byte-compile-dest-file slime-helper-el))

(when (and (file-exists-p slime-helper-el)
           (or (not (file-exists-p slime-helper-elc))
               (file-newer-than-file-p slime-helper-el
                                       slime-helper-elc)))
  (byte-compile-file slime-helper-el t)
  (shell-command (concat "cd \"" slime-path
                         "\" ; make compile contrib-compile")))

(when (and (file-exists-p slime-helper-elc)
           (not (find 'slime features)))
  (load-file slime-helper-elc))

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
 gc-cons-threshold                 10240000 ; garbage collection every 10 Mb
 gnus-permanently-visible-groups   ""      ; always show all groups
 indent-tabs-mode                  nil     ; identation only with spaces
 inferior-lisp-program             "sbcl"  ; SBCL
 inhibit-startup-screen            t       ; remove welcome screen
 initial-scratch-message           ";; Lisp Interaction\n\n" ; scratch msg
 kill-read-only-ok                 t       ; don't rise errors, it's OK
 large-file-warning-threshold      10240000 ; warn when opening >10 Mb file
 make-backup-files                 nil     ; don't create backups
 major-mode                        'text-mode ; default mode is text mode
 minibuffer-eldef-shorten-default  t       ; shorten defaults in minibuffer
 python-indent-guess-indent-offset nil     ; don't guess indent offset
 python-indent-offset              4       ; indent offset for python mode
 require-final-newline             t       ; always requite it
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

(delete-selection-mode             1) ; delete selection mode enabled
(display-time-mode                 1) ; displaying time
(menu-bar-mode                    -1) ; hide menu bar
(minibuffer-electric-default-mode  1) ; electric minibuffer
(put 'downcase-region  'disabled nil) ; don't ask anything when I use it
(put 'erase-buffer     'disabled nil) ; see above
(put 'upcase-region    'disabled nil) ; see above
(scroll-bar-mode                  -1) ; disable scroll bar
(show-paren-mode                   1) ; highlight parenthesis
(tool-bar-mode                    -1) ; hide tool bar
(smooth-scroll-mode                t) ; smooth scroll
(which-function-mode               1) ; displays current function
(global-auto-revert-mode           1) ; revert buffers automatically
;; open .pl files as Prolog files, not Perl files
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                Bindings                                ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar basic-buffers
  '("*scratch*"
    "*Messages*"
    "irc.freenode.net:6667"
    "#lisp"
    "#emacs")
  "These are the buffers that I don't want to purge with
PURGE-BUFFERS command.")

(defun purge-buffers ()
  "Kill all buffer except those that have names listed in
BASIC-BUFFERS."
  (interactive)
  (dolist (x (remove-if (lambda (x)
                          (find (buffer-name x)
                                basic-buffers
                                :test #'string-equal))
                        (buffer-list)))
    (kill-buffer x)
    (message (buffer-name x)))
  (switch-to-buffer (car basic-buffers))
  (delete-other-windows))

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

(defun search-online ()
  "Search Internet with DuckDuckGo."
  (interactive)
  (browse-url
   (concat "https://duckduckgo.com/html/?k1=-1&q="
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning)
                                  (region-end))
              (read-string "DuckDuckGo: "))))))

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
(global-set-key (kbd "C-c s")   'search-online)
(eval-after-load "slime"
  '(progn
     (define-key slime-repl-mode-map
       (kbd "C-c r") 'slime-restart-inferior-lisp)))
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
;;                                Aliases                                 ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'qr 'query-replace)
(defalias 'sh 'shell)
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                 Hooks                                  ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hidden-minor-modes
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
  "Puts empty strings for minor modes in HIDDEN-MINOR-MODES into
MINOR-MODE-ALIST, effectively evicting them from the status
line."
  (dolist (x hidden-minor-modes)
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
  "The name of the function speaks for itself."
  (electric-indent-local-mode 0))

(add-hook 'after-change-major-mode-hook 'fci-mode)
(add-hook 'after-change-major-mode-hook 'purge-minor-modes)
(add-hook 'before-save-hook             'delete-trailing-whitespace)
(add-hook 'clojure-mode-hook            'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook         'rainbow-delimiters-mode)
(add-hook 'emacs-startup-hook           'purge-buffers)
(add-hook 'haskell-mode-hook            'electric-indent-disable-locally)
(add-hook 'haskell-mode-hook            'inf-haskell-mode)
(add-hook 'haskell-mode-hook            'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook            'turn-on-haskell-indent)
(add-hook 'prog-mode-hook               'prepare-prog-mode)
(add-hook 'scheme-mode-hook             'rainbow-delimiters-mode)
(add-hook 'slime-mode-hook              'rainbow-delimiters-mode)
(add-hook 'text-mode-hook               'auto-fill-mode)
(add-hook 'text-mode-hook               'flyspell-mode)

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
      cider-repl-display-in-current-window t
      cider-repl-result-prefix             ";; => ")
