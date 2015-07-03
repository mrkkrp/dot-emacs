;;; mk-utils.el --- Various utility functions -*- lexical-binding: t; -*-
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

;; I've collected various auxiliary functions here to avoid cluttering of
;; other files.

;;; Code:

(require 'cl-lib)
(require 'f)

(defun mk-shell-quote-arg (arg)
  "Quote ARG for using in shell.
This function is different from `shell-quote-argument' in that it
can be used for text transformations in Yasnippet without
backslash flood."
  (replace-regexp-in-string "\\W" "\\\\\\&" (remove ?\\ arg)))

(defun transpose-line-down (&optional arg)
  "Move current line and cursor down.
Argument ARG, if supplied, specifies how many times the operation
should be performed."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (let ((col (current-column)))
      (forward-line    1)
      (transpose-lines 1)
      (forward-line   -1)
      (move-to-column col))))

(defun transpose-line-up (&optional arg)
  "Move current line and cursor up.
Argument ARG, if supplied, specifies how many times the operation
should be performed."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (let ((col (current-column)))
      (transpose-lines 1)
      (forward-line   -2)
      (move-to-column col))))

(defun duplicate-line (&optional arg)
  "Copy current line and yank its copy under the current line.
Position of point shifts one line down.  Argument ARG, if
supplied, specifies how many times the operation should be
performed."
  (interactive "p")
  (dotimes (_ (or arg 1))
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
      (move-to-column col))))

(defun yank-primary ()
  "Insert contents of the primary selection at the point."
  (interactive)
  (insert (gui-get-selection)))

(defun file-name-to-kill-ring (arg)
  "Put name of file into kill ring.
If user's visiting a buffer that's associated with a file, use
name of the file.  If major mode is `dired-mode', use name of
file at point, but if point is not placed at any file, put name
of actual directory into kill ring.  Argument ARG, if given,
makes result string be quoted as for yanking into shell."
  (interactive "P")
  (let ((φ (if (find major-mode
                     '(dired-mode wdired-mode))
               (or (dired-get-filename nil t)
                   default-directory)
             (buffer-file-name))))
    (when φ
      (message "%s → kill ring"
               (kill-new
                (expand-file-name
                 (if arg
                     (shell-quote-argument φ)
                   φ)))))))

(defun mark-rest-of-line ()
  "Set region from point to end of current line."
  (interactive)
  (set-mark
   (save-excursion
     (move-end-of-line 1)
     (point))))

(defun copy-buffer ()
  "Put entire buffer into the kill ring."
  (interactive)
  (kill-new (buffer-string)))

(defun mk-first-line (&optional arg)
  "Go to beginning of current buffer and ARG lines down."
  (interactive "p")
  (goto-char (point-min))
  (forward-line (or arg 0)))

(defun mk-last-line (&optional arg)
  "Go to end of current buffer and ARG lines up."
  (interactive "p")
  (goto-char (point-max))
  (forward-line (- (or arg 0))))

(defvar keyboard-key-name
  '(("alt"       . "⎇")
    ("backspace" . "⌫")
    ("caps lock" . "⇪")
    ("command"   . "⌘")
    ("control"   . "⎈")
    ("ctrl"      . "⎈")
    ("del"       . "⌦")
    ("delete"    . "⌦")
    ("down"      . "↓")
    ("end"       . "↘")
    ("enter"     . "↵")
    ("esc"       . "⎋")
    ("escape"    . "⎋")
    ("home"      . "↖")
    ("left"      . "←")
    ("menu"      . "▤")
    ("meta"      . "◆")
    ("option"    . "⌥")
    ("page down" . "⇟")
    ("page up"   . "⇞")
    ("return"    . "↵")
    ("right"     . "→")
    ("shift"     . "⇧")
    ("tab"       . "↹")
    ("up"        . "↑")
    ("windows"   . "❖"))
  "Names of various keys on the keyboard.
It's used in `insert-key-name' function.")

(defun insert-key-name (key-name)
  "Insert name of keyboard key KEY-NAME."
  (interactive
   (list (completing-read "Key name: "
                          (mapcar #'car keyboard-key-name))))
  (let ((ξ (cdr (assoc (string-trim (downcase key-name)) keyboard-key-name))))
    (insert (concat "<kbd>" ξ (when ξ " ") (capitalize key-name) "</kbd>"))))

(defun show-date (&optional stamp)
  "Show current date in the minibuffer.
If STAMP is not NIL, insert date at point."
  (interactive)
  (funcall (if stamp #'insert #'message)
           (format-time-string "%A, %e %B %Y")))

(defun show-default-dir ()
  "Show default directory in the minibuffer."
  (interactive)
  (message (expand-file-name default-directory)))

(defvar quick-to-die nil
  "List of major modes that are preferred to be killed, not buried.")

(defvar preferred-death nil
  "Alist describing how to kill buffers with various major modes.")

(defun kill-or-bury-alive (&optional arg)
  "Kill or bury current buffer.

This is universal killing mechanism.  When argument ARG is given
and it's not NIL, kill current buffer.  Otherwise behavior of
this command varies.  If current buffer has major mode listed in
`quick-to-die' list, kill it immediately, otherwise just bury it.

Various major modes can be killed differently.  Add a
pair (major-mode-name . killing-function) to `preferred-death'
variable to choose something fancy.  Standard `kill-buffer' is
used as fallback."
  (interactive "P")
  (if (or arg (find major-mode quick-to-die))
      (funcall (or (cdr (assoc major-mode preferred-death))
                   (kill-buffer)))
    (bury-buffer)))

(defvar mk-basic-buffers
  '("^\*scratch\*"
    "^\*Messages\*"
    "^\*Compile-Log\*"
    "^irc\.freenode\.net:6667"
    "^#.+")
  "These are regexps to match names of buffers that I don't want to purge.")

(defun purge-buffers ()
  "Kill all buffers except for those that match regexps in `basic-buffers'."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((buffer-name (buffer-name buffer)))
      (when (and buffer-name
                 (notany (lambda (regexp)
                           (string-match-p regexp
                                           buffer-name))
                         mk-basic-buffers))
        (kill-buffer buffer))))
  (switch-to-buffer "*scratch*")
  (delete-other-windows))

(defun mk-grab-input (prompt &optional initial-input add-space)
  "Grab input from user.
If there is an active region, use its contents, otherwise read
text from the minibuffer.  PROMPT is a prompt to show,
INITIAL-INPUT is the initial input.  If INITIAL-INPUT and
ADD-SPACE are not NIL, add one space after the initial input."
  (if mark-active
      (buffer-substring (region-beginning)
                        (region-end))
    (read-string prompt
                 (concat initial-input
                         (when (and initial-input add-space) " ")))))

(defvar mk-search-prefix nil
  "This is an alist that contains some prefixes for online search query.
Prefixes are picked up according to currect major mode.")

(defun mk-search (what)
  "Search Internet for WHAT thing, with DuckDuckGo.
When called interactively, it uses prefix corresponding to
current major mode, as specified in `mk-search-prefix'."
  (interactive
   (list (mk-grab-input "DuckDuckGo: "
                        (cdr (assoc major-mode
                                    mk-search-prefix))
                        t)))
  (browse-url
   (concat "https://duckduckgo.com/html/?k1=-1&q="
           (url-hexify-string what))))

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                (let ((pkg (cadr (assq name where))))
                  (when pkg
                    (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date."))))

(defun pkgi-filter-args (args)
  "How to filter arguments of `package-install' command.
First element of ARGS is passed to the command, while the second
is always T, so one can select any packages only by manually
adding them to `package-selected-packages' variable."
  (list (car args) t))

(defun compile-init-files ()
  "Byte compile init files (all *.el files under `mk-dir' directory)."
  (interactive)
  (let (once)
    (save-window-excursion
      (dolist (item (cons user-init-file
                          (directory-files mk-dir t "\\`[^#].*\\.el\\'" t)))
        (let ((compiled (byte-compile-dest-file item)))
          (when (or (not (file-exists-p compiled))
                    (file-newer-than-file-p item compiled))
            (byte-compile-file item)
            (setq once t)))))
    (unless once
      (message "Byte compiled init files exist and are up to date."))))

(defun mk-eval-last-sexp ()
  "Evaluate last S-expression and replace it with the result."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(defun mk-visit-file (filename)
  "Visit specified file FILENAME.
If the file does not exist, print a message about the fact, but
don't create new empty buffer."
  (let ((filename (expand-file-name filename)))
    (if (file-exists-p filename)
        (find-file filename)
      (message "%s does not exist." filename))))

(defun mk-double-buffer ()
  "Show currect buffer in other window and switch to that window."
  (interactive)
  (if (> (length (window-list)) 1)
      (let ((original-buffer (buffer-name)))
        (other-window 1)
        (switch-to-buffer original-buffer))
    (split-window-sensibly)
    (other-window 1)))

(defun mk-switch-theme (theme)
  "Switch to theme THEME, loading it if necessary.
This command disables all enabled themes before loading theme
THEME.  This is what you usually want."
  (interactive
   (list
    (intern
     (completing-read "Switch to theme: "
                      (mapcar 'symbol-name
                              (custom-available-themes))))))
  (dolist (enabled-theme custom-enabled-themes)
    (disable-theme enabled-theme))
  (load-theme theme t))

(defun mk-set-font (font &optional height)
  "Set font FONT as main font for all frames.
HEIGHT, if supplied, specifies height of letters to use."
  (interactive
   (list (completing-read "Use font: " (font-family-list)) nil))
  (set-face-attribute 'default nil :family font)
  (when height
    (set-face-attribute 'default nil :height height))
  (set-face-attribute 'variable-pitch nil :family font))

(defun mk-use-lang (input-method dictionary)
  "Switch between input methods and Ispell dictionaries.
Switch between given INPUT-METHOD and DICTIONARY and their defaults."
  (if (eq current-input-method input-method)
      (progn
        (deactivate-input-method)
        (ispell-change-dictionary "default"))
    (set-input-method input-method)
    (ispell-change-dictionary dictionary)))

(defmacro translate-kbd (from to)
  "Translate combinations of keys FROM to TO combination.
Effect of this translation is global."
  `(define-key key-translation-map (kbd ,from) (kbd ,to)))

(defvar minor-mode-alias nil
  "Alias for minor modes.")

(defvar major-mode-alias nil
  "Alias for major modes.")

(defun apply-mode-alias ()
  "Use alias from `minor-mode-alias' and `major-mode-alias'."
  (dolist (x minor-mode-alias)
    (let ((trg (cdr (assoc (car x) minor-mode-alist))))
      (when trg
        (setcar trg (cdr x)))))
  (let ((mode-alias (cdr (assoc major-mode major-mode-alias))))
    (when mode-alias
      (setq mode-name mode-alias))))

(defun mk-find-file (regexp)
  "Find file whose name satisfies REGEXP traversing upwards.
Return absolute path to directory containing that file or NIL on
failure."
  (f-traverse-upwards
   (lambda (path)
     (directory-files path t regexp t))
   default-directory))

(defun mk-make ()
  "Find makefile of current project and execute `make'."
  (interactive)
  (let ((dir (mk-find-file "\\`[Mm]akefile\\'")))
    (if dir
        (compile
         (format "cd %s ; make -k"
                 (shell-quote-argument dir)))
      (message "Cannot find makefile for this project."))))

(defun mk-install ()
  "Find `install.sh' script of current project and execute it.
`sudo' is used automatically, you'll need to enter your `sudo'
password."
  (interactive)
  (let ((dir (mk-find-file "\\`install.sh\\'")))
    (if dir
        (save-window-excursion
          (compile
           (format "cd %s ; sudo sh install.sh"
                   (shell-quote-argument dir))
           t))
      (message "Cannot find ‘install.sh’ file for this project."))))

(defun mk-uninstall ()
  "Find `uninstall.sh' script of current project and execute it.
`sudo' is used automatically, you'll need to enter your `sudo'
password."
  (interactive)
  (let ((dir (mk-find-file "\\`uninstall.sh\\'")))
    (if dir
        (save-window-excursion
          (compile
           (format "cd %s ; sudo sh uninstall.sh"
                   (shell-quote-argument dir))
           t))
      (message "Cannot find ‘uninstall.sh’ file for this project."))))

(defun exit-emacs (&optional arg)
  "Exit Emacs: save all file-visiting buffers, kill terminal.
If ARG is given and it's not NIL, don't ask user if he wants to
exit."
  (interactive "P")
  (when (or arg (yes-or-no-p "Exit Emacs?"))
    (save-buffers-kill-terminal)))

;; My little helpers from Greece…

(defmacro σ (&rest args)
  "Return function that returns list of ARGS."
  `(lambda (&rest _rest)
     (list ,@args)))

(defmacro ε (fnc &rest args)
  "Interactively invoke function FNC with arguments ARGS.
Kind of partial application."
  `(lambda (&rest rest)
     (interactive)
     (apply ,fnc ,@args rest)))

(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

(defun π (key fnc)
  "Set global key binding that binds KEY to FNC."
  (global-set-key (kbd key) fnc))

(defmacro τ (file keymap key fnc)
  "When FILE is loaded, add to KEYMAP key binding KEY (invoking FNC)."
  `(eval-after-load ',file
     '(define-key
        (symbol-value (intern (concat (symbol-name ',keymap) "-mode-map")))
        (kbd ,key) ,fnc)))

(provide 'mk-utils)

;;; mk-utils.el ends here
