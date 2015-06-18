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

(defun shell-quote-arg (arg)
  "Quote ARG for using in shell.
This function is different from `shell-quote-argument' in that it
can be invoked repeatedly in Yasnippet without backslash flood.
Don't use it when you need to fill out long input, because it
reiterates all the text at every key press."
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

(defun mark-rest-of-line ()
  "Set region from point to end of current line."
  (interactive)
  (set-mark
   (save-excursion
     (move-end-of-line 1)
     (point))))

(defun copy-buffer ()
  "Copy entire buffer into the clipboard."
  (interactive)
  (kill-new (buffer-string)))

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

(defun insert-key-name ()
  "Read a key from the keyboard and insert its name."
  (interactive)
  (let* ((β (ido-completing-read "Key name: "
                                 (mapcar #'car keyboard-key-name)))
         (ξ (cdr (assoc (string-trim (downcase β)) keyboard-key-name))))
    (insert (concat "<kbd>" ξ (when ξ " ") (capitalize β) "</kbd>"))))

(defun show-date (&optional stamp)
  "Show current date in the minibuffer.
If STAMP is not NIL, insert date into currently active buffer."
  (interactive)
  (funcall (if stamp #'insert #'message)
           (format-time-string "%A, %e %B %Y")))

(defun show-default-dir ()
  "Show default directory in the minibuffer."
  (interactive)
  (message (expand-file-name default-directory)))

(defvar basic-buffers
  '("^\*scratch\*"
    "^\*Messages\*"
    "^irc\.freenode\.net:6667"
    "^#.+")
  "These are regexps to match names of buffers that I don't want to purge.")

(defun purge-buffers ()
  "Kill all buffers except for those that have names listed in `basic-buffers'."
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

(defun grab-input (prompt &optional initial-input add-space)
  "Grab input from user.
If there is an active region, use its contents.  PROMPT is a
prompt to show, INITIAL-INPUT is the initial input.  If
INITIAL-INPUT and ADD-SPACE are not NIL, add one space after the
initial input."
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
   (list (grab-input "DuckDuckGo: "
                     (cdr (assoc major-mode
                                 mk-search-prefix))
                     t)))
  (browse-url
   (concat "https://duckduckgo.com/html/?k1=-1&q="
           (url-hexify-string what))))

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing any special buffer."
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
        (save-window-excursion
          (dolist (package-desc upgrades)
            (let ((old-package (cadr (assq (package-desc-name package-desc)
                                           package-alist))))
              (package-install package-desc)
              (package-delete  old-package))))))))

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
                          (directory-files mk-dir t "\\`.*\\.el\\'" t)))
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

(defun visit-file (filename)
  "Visit specified file FILENAME.
If the file does not exist, print a message about the fact."
  (let ((filename (expand-file-name filename)))
    (if (file-exists-p filename)
        (find-file filename)
      (message "%s does not exist." filename))))

(defun double-buffer ()
  "Show currect buffer in other window."
  (interactive)
  (if (> (length (window-list)) 1)
      (let ((original-buffer (buffer-name)))
        (other-window 1)
        (switch-to-buffer original-buffer))
    (split-window-sensibly)
    (other-window 1)))

(defun mk-switch-theme (theme)
  "Switch to theme THEME, loading it if necessairy."
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

(provide 'mk-utils)

;;; mk-utils.el ends here
