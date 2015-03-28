;;; .emacs --- Emacs configuration file (GNU Emacs 24.4.1)
;;;
;;; Commentary:
;;;
;;; In order to use spell-checking you need to install the following
;;; packages (OS level):
;;; * aspell{,-en,-ru,-fr}
;;;
;;; For Haskell development install (with cabal):
;;; * happy
;;; * alex
;;; * ghc-mod
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

(require 'cl)
(require 'package)
(require 'bytecomp)

(add-to-list
 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(defvar vital-packages
  '(ace-window
    buffer-move
    cider
    color-theme
    fill-column-indicator
    flycheck
    flycheck-haskell
    ghc
    haskell-mode
    magit
    markdown-mode
    prolog
    rainbow-delimiters
    smooth-scroll
    solarized-theme)
  "List of packages that must be installed.")

;; Install all the packages automatically if they are not installed.

(unless package-archive-contents
  (package-refresh-contents))

(defun delete-window-by-name (name)
  "Delete all windows that display buffer with name NAME."
  (when (get-buffer name)
    (dolist (window (get-buffer-window-list name))
      (delete-window window))))

(dolist (package vital-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'smooth-scroll)

;; Let's load SLIME with Slime Helper, if there is `slime-helper.el' file,
;; we byte-compile it and entire SLIME, and next time we will be able to
;; load SLIME faster. If there is more recent version of `slime-helper.el'
;; available, we should recompile it (and SLIME).

(defvar slime-helper-el  (expand-file-name "~/quicklisp/slime-helper.el")
  "Path to SLIME helper that comes with Quicklisp.")
(defvar slime-helper-elc (byte-compile-dest-file slime-helper-el)
  "Path to byte-compiled SLIME helper.")

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

;; Clearing after compilation...

(delete-window-by-name "*Compile-Log*")
(delete-window-by-name "*Shell Command Output*")

(require 'server)

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
 browse-url-browser-function       'browse-url-generic ; use GNU IceCat
 browse-url-generic-program        "icecat" ; GNU IceCat
 column-number-mode                t       ; display column number
 delete-by-moving-to-trash         t       ; in dired mode
 dired-auto-revert-buffer          t       ; automatically revert buffer
 dired-dwim-target                 t       ; guess target directory
 dired-keep-marker-copy            nil     ; don't mark copied files
 dired-listing-switches            "-GAlh" ; ls command arguments
 dired-recursive-copies            'always ; don't ask me, just do it
 dired-recursive-deletes           'always ; see above
 display-time-24hr-format          t       ; 24 hours format for time
 erc-nick                          "mrkkrp"
 fci-rule-column                   80      ; position of rule column
 fill-column                       76      ; set fill column
 gc-cons-threshold                 10240000 ; garbage collection every 10 Mb
 gnus-permanently-visible-groups   ""      ; always show all groups
 haskell-ask-also-kill-buffers     nil     ; don't ask
 haskell-process-show-debug-tips   nil     ; don't show anything
 indent-tabs-mode                  nil     ; identation only with spaces
 inferior-lisp-program             "sbcl"  ; SBCL
 inhibit-startup-screen            t       ; remove welcome screen
 initial-scratch-message           ";; Lisp Interaction\n\n" ; scratch msg
 kill-read-only-ok                 t       ; don't rise errors, it's OK
 large-file-warning-threshold      10240000 ; warn when opening >10 Mb file
 major-mode                        'text-mode ; default mode is text mode
 make-backup-files                 nil     ; don't create backups
 Man-width                         fill-column ; fill column for man pages
 minibuffer-eldef-shorten-default  t       ; shorten defaults in minibuffer
 org-agenda-files                  '("~/todo.org")
 org-catch-invisible-edits         'show   ; make point visible
 python-indent-guess-indent-offset nil     ; don't guess indent offset
 python-indent-offset              4       ; indent offset for python mode
 require-final-newline             t       ; always requite it
 resize-mini-windows               t       ; grow and shrink
 ring-bell-function                'ignore ; no annoying alarms
 scroll-step                       1       ; convenient scrolling
 send-mail-function                'smtpmail-send-it
 smtpmail-smtp-server              "smtp.openmailbox.org"
 smtpmail-smtp-service             587
 suggest-key-bindings              nil
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

(blink-cursor-mode                 0) ; my cursor doesn't blink
(delete-selection-mode             1) ; delete selection mode enabled
(display-time-mode                 1) ; displaying time
(global-auto-revert-mode           1) ; revert buffers automatically
(menu-bar-mode                    -1) ; hide menu bar
(minibuffer-electric-default-mode  1) ; electric minibuffer
(put 'downcase-region  'disabled nil) ; don't ask anything when I use it
(put 'erase-buffer     'disabled nil) ; see above
(put 'upcase-region    'disabled nil) ; see above
(scroll-bar-mode                  -1) ; disable scroll bar
(show-paren-mode                   1) ; highlight parenthesis
(smooth-scroll-mode                t) ; smooth scroll
(tool-bar-mode                    -1) ; hide tool bar
(which-function-mode               1) ; displays current function
;; open .pl files as Prolog files, not Perl files
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                Bindings                                ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun point-mid ()
  "Return middle position of point in the buffer."
  (/ (- (point-max) (point-min)) 2))

(defun transpose-line-down ()
  "Move current line and cursor down."
  (interactive)
  (let ((col (current-column)))
    (forward-line    1)
    (transpose-lines 1)
    (forward-line   -1)
    (move-to-column col)))

(defun transpose-line-up ()
  "Move current line and cursor up."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line   -2)
    (move-to-column col)))

(defun duplicate-line ()
  "Copy current line and yank its copy under the current
line. Position of point shifts one line down."
  (interactive)
  (let ((col (current-column)))
    (kill-ring-save
     (progn
       (move-beginning-of-line 1)
       (point))
     (progn
       (forward-line 1)
       (point)))
    (yank)
    (forward-line -1)
    (move-to-column col)))

(defun show-date (&optional stamp)
  "Show current date in the minibuffer. If STAMP is not NIL,
insert date into currently active buffer."
  (interactive)
  (funcall (if stamp #'insert #'message)
           (format-time-string "%A, %d %B %Y")))

(defvar basic-buffers
  '("^\*scratch\*"
    "^\*Messages\*"
    "^irc\.freenode\.net:6667"
    "^#.?")
  "These are regexps to match names of buffers that I don't want
to purge with PURGE-BUFFERS command.")

(defun purge-buffers ()
  "Kill all buffer except those that have names listed in
BASIC-BUFFERS."
  (interactive)
  (let ((redundant-buffers
         (remove-if (lambda (name)
                      (some (lambda (regexp)
                              (string-match-p regexp name))
                            basic-buffers))
                    (mapcar #'buffer-name (buffer-list)))))
    (mapc (lambda (name)
            (kill-buffer
             (if (get-buffer name)
                 name
               (subseq name 0 (or (position ?< name :from-end t)
                                  (length name))))))
          redundant-buffers)
    (switch-to-buffer "*scratch*")
    (delete-other-windows)))

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

(defun upgrade-all-packages ()
  "Upgrade all packages automatically."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                (package-desc-version (cadr (assq name where)))))
      (dolist (package (mapcar #'car package-alist))
        (when (version-list-< (get-version package package-alist)
                              (get-version package package-archive-contents))
          (push (cadr (assq package package-archive-contents))
                upgrades))))
    (if (null upgrades)
        (message "All packages are up to date.")
      (when (yes-or-no-p
             (message "Upgrade %d package%s (%s)? "
                      (length upgrades)
                      (if (= (length upgrades) 1) "" "s")
                      (mapconcat #'package-desc-full-name upgrades ", ")))
        (dolist (package-desc upgrades)
          (let ((old-package (cadr (assq (package-desc-name package-desc)
                                         package-alist))))
            (package-install package-desc)
            (package-delete  old-package)))
        (delete-window-by-name "*Compile-Log*")))))

(defun compile-init-file ()
  (interactive)
  (let ((compiled (byte-compile-dest-file user-init-file)))
    (if (or (not (file-exists-p compiled))
            (file-newer-than-file-p user-init-file
                                    compiled))
        (progn
          (byte-compile-file user-init-file)
          (delete-window-by-name "*Compile-Log*"))
      (message "Byte compiled init file exists and it's up to date."))))

(defun visit-file (filename)
  "Visit specified file FILENAME. If the file does not exist,
print a message about the fact."
  (let ((filename (expand-file-name filename)))
    (if (file-exists-p filename)
        (find-file filename)
      (message (concat filename " does not exist.")))))

(defun toggle-russian-input ()
  "Switch between Russian input method and normal input method."
  (interactive)
  (if current-input-method
      (progn
        (deactivate-input-method)
        (ispell-change-dictionary "default"))
    (set-input-method 'russian-computer)
    (ispell-change-dictionary "ru")))

(defun slime-in-package ()
  "Load specified package and switch to it."
  (interactive)
  (let ((pkg-name (read-string "Package name: ")))
    (slime-repl-eval-string
     (concat "(progn (asdf:load-system :"
             pkg-name
             ")(cl:in-package :"
             pkg-name
             "))"))))

(defmacro cmd (fnc &rest args)
  "Interactively invoke function FNC with arguments ARGS."
  `(lambda (&rest rest)
     (interactive)
     (apply,fnc ,@args rest)))

(global-set-key (kbd "C-c c") #'comment-region)
(global-set-key (kbd "C-c u") #'uncomment-region)
(global-set-key (kbd "C-c r") #'revert-buffer)
(global-set-key (kbd "C-c p") #'purge-buffers)
(global-set-key (kbd "C-c s") #'search-online)
(global-set-key (kbd "C-c g") #'upgrade-all-packages)
(global-set-key (kbd "C-c b") #'compile-init-file)
(global-set-key (kbd "C-c e") (cmd #'visit-file user-init-file))
(global-set-key (kbd "C-c t") (cmd #'visit-file (car org-agenda-files)))
(global-set-key (kbd "C-c a") #'org-agenda-list)
(global-set-key (kbd "C-x o") #'ace-window)
(global-set-key (kbd "C-c i") #'flyspell-correct-word-before-point)
(global-set-key (kbd "M-p")   #'transpose-line-up)
(global-set-key (kbd "M-n")   #'transpose-line-down)
(global-set-key (kbd "<f2>")  #'save-buffer)
(global-set-key (kbd "<f5>")  #'find-file)
(global-set-key (kbd "<f6>")  #'find-file-other-window)
(global-set-key (kbd "<f8>")  #'toggle-russian-input)
(global-set-key (kbd "<f9>")  (cmd #'kill-buffer nil))
(global-set-key (kbd "<f10>") #'delete-other-windows)
(global-set-key (kbd "<f11>") #'switch-to-buffer)
(global-set-key (kbd "<f12>") #'save-buffers-kill-terminal)
(global-set-key (kbd "<escape>")   #'delete-window)
(global-set-key (kbd "<C-return>") #'duplicate-line)
(global-set-key (kbd "<S-up>")     #'buf-move-up)
(global-set-key (kbd "<S-down>")   #'buf-move-down)
(global-set-key (kbd "<S-left>")   #'buf-move-left)
(global-set-key (kbd "<S-right>")  #'buf-move-right)
(global-set-key (kbd "<menu>")     nil)
(global-set-key (kbd "<menu> ,")   (cmd #'push-mark))
(global-set-key (kbd "<menu> .")   (cmd #'goto-char (mark)))
(global-set-key (kbd "<menu> /")   (cmd #'goto-char (point-mid)))
(global-set-key (kbd "<menu> <")   (cmd #'goto-char (point-min)))
(global-set-key (kbd "<menu> >")   (cmd #'goto-char (point-max)))
(global-set-key (kbd "<menu> a p") #'apropos)
(global-set-key (kbd "<menu> c a") #'calc)
(global-set-key (kbd "<menu> c i") #'cider-jack-in)
(global-set-key (kbd "<menu> c l") #'calendar)
(global-set-key (kbd "<menu> d a") (cmd #'show-date))
(global-set-key (kbd "<menu> e r") #'erc)
(global-set-key (kbd "<menu> g d") #'gdb)
(global-set-key (kbd "<menu> g l") #'goto-line)
(global-set-key (kbd "<menu> g n") #'gnus)
(global-set-key (kbd "<menu> h r") #'split-window-below)
(global-set-key (kbd "<menu> l i") #'slime)
(global-set-key (kbd "<menu> l p") #'list-packages)
(global-set-key (kbd "<menu> m a") #'magit-status)
(global-set-key (kbd "<menu> m n") #'man)
(global-set-key (kbd "<menu> q r") #'query-replace)
(global-set-key (kbd "<menu> s c") #'run-scheme)
(global-set-key (kbd "<menu> s h") #'shell)
(global-set-key (kbd "<menu> s l") #'sort-lines)
(global-set-key (kbd "<menu> s s") (cmd #'switch-to-buffer "*scratch*"))
(global-set-key (kbd "<menu> s t") (cmd #'show-date t))
(global-set-key (kbd "<menu> t e") #'tetris)
(global-set-key (kbd "<menu> v r") #'split-window-right)

(defmacro defkey (file keymap key def)
  "Little helper to write mode-specific key definitions prettier."
  `(eval-after-load ',file
     '(define-key
        (symbol-value (intern (concat (symbol-name ',keymap) "-mode-map")))
        (kbd ,key) ,def)))

(defkey cc-mode       c                "C-c C-l" #'compile)
(defkey haskell    haskell-interactive "C-c h"   #'haskell-hoogle)
(defkey haskell-cabal haskell-cabal    "M-n"     #'transpose-line-down)
(defkey haskell-cabal haskell-cabal    "M-p"     #'transpose-line-up)
(defkey haskell-mode  haskell          "C-c h"   #'haskell-hoogle)
(defkey lisp-mode     emacs-lisp       "C-c h"   #'slime-hyperspec-lookup)
(defkey lisp-mode     lisp             "C-c h"   #'slime-hyperspec-lookup)
(defkey slime         slime            "M-n"     #'transpose-line-down)
(defkey slime         slime            "M-p"     #'transpose-line-up)
(defkey slime         slime-repl       "C-c i"   #'slime-in-package)
(defkey slime         slime-repl       "C-c r"   #'slime-restart-inferior-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                                Aliases                                 ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'yes-or-no-p #'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                             Hooks & Advice                             ;;
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

(defun haskell-mode-helper ()
  "Some auxiliary things for Haskell support."
  (electric-indent-local-mode 0)
  (interactive-haskell-mode)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent))

(add-hook 'after-change-major-mode-hook #'fci-mode)
(add-hook 'after-change-major-mode-hook #'purge-minor-modes)
(add-hook 'before-save-hook             #'delete-trailing-whitespace)
(add-hook 'clojure-mode-hook            #'rainbow-delimiters-mode)
(add-hook 'dired-mode-hook              #'hl-line-mode)
(add-hook 'emacs-lisp-mode-hook         #'rainbow-delimiters-mode)
(add-hook 'erc-mode-hook                #'flyspell-mode)
(add-hook 'flycheck-mode-hook           #'flycheck-haskell-setup)
(add-hook 'gnus-group-mode-hook         #'hl-line-mode)
(add-hook 'gnus-summary-mode-hook       #'hl-line-mode)
(add-hook 'haskell-mode-hook            #'haskell-mode-helper)
(add-hook 'prog-mode-hook               #'prepare-prog-mode)
(add-hook 'scheme-mode-hook             #'rainbow-delimiters-mode)
(add-hook 'slime-mode-hook              #'rainbow-delimiters-mode)
(add-hook 'text-mode-hook               #'auto-fill-mode)
(add-hook 'text-mode-hook               #'flyspell-mode)

(defmacro ira (&rest args)
  "Make lambda that always interactively returns list of this
macro's arguments ignoring any arguments passed to it."
  `(lambda (&rest rest)
     (interactive)
     ',args))

(advice-add 'org-agenda-todo :after       #'org-save-all-org-buffers)
(advice-add 'revert-buffer   :filter-args (ira nil t))
(advice-add 'compile         :filter-args (ira "cd .. ; make -k"))
(advice-add 'save-buffers-kill-terminal :filter-args (ira t))
(advice-add 'haskell-session-new-assume-from-cabal :override (lambda ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;                        Only Under Window System                        ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (set-face-attribute 'default
                      nil
                      :family "Ubuntu Mono"
                      :height 120)
  (set-face-attribute 'variable-pitch
                      nil
                      :family "Ubuntu Mono")
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
