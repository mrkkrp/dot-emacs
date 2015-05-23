;;; mk-utils.el --- Various utility functions -*- lexical-binding: t; -*-
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

;;; I've collected various auxiliary functions here, to avoid cluttering of
;;; other files.

;;; Code:

(defun delete-window-by-name (name)
  "Delete all windows that display buffer with name NAME.
Note that the buffer itself is not killed."
  (when (get-buffer name)
    (dolist (window (get-buffer-window-list name))
      (delete-window window))))

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
  "Copy current line and yank its copy under the current line.
Position of point shifts one line down."
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

(defun yank-primary ()
  "Insert the primary selection at the point."
  (interactive)
  (insert (gui-get-selection)))

(defun mark-rest-of-line ()
  "Set region from point to end of current line."
  (interactive)
  (set-mark (point))
  (move-end-of-line 1))

(defun copy-buffer ()
  "Copy entire buffer into the clipboard."
  (interactive)
  (kill-new (buffer-string)))

(defun insert-key-name ()
  "Read a key from the keyboard and insert its name."
  (interactive)
  (let* ((items '(("alt"         . "⎇")
                  ("backspace"   . "⌫")
                  ("caps lock"   . "⇪")
                  ("command"     . "⌘")
                  ("control"     . "⎈")
                  ("ctrl"        . "⎈")
                  ("del"         . "⌦")
                  ("delete"      . "⌦")
                  ("down arrow"  . "↓")
                  ("end"         . "↘")
                  ("enter"       . "↵")
                  ("esc"         . "⎋")
                  ("escape"      . "⎋")
                  ("home"        . "↖")
                  ("left arrow"  . "←")
                  ("menu"        . "▤")
                  ("meta"        . "◆")
                  ("option"      . "⌥")
                  ("page down"   . "⇟")
                  ("page up"     . "⇞")
                  ("return"      . "↵")
                  ("right arrow" . "→")
                  ("shift"       . "⇧")
                  ("tab"         . "↹")
                  ("up arrow"    . "↑")
                  ("windows"     . "❖")))
         (β (ido-completing-read "Key name: " (mapcar #'car items)))
         (ξ (cdr (assoc (string-trim (downcase β)) items))))
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
  "Kill all buffer except those that have names listed in BASIC-BUFFERS."
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

(defun compile-init-files ()
  "Byte compile init files."
  (interactive)
  (let (once)
    (dolist (item (cons user-init-file
                        (directory-files mk-dir t "\\`.*\\.el\\'" t)))
      (let ((compiled (byte-compile-dest-file item)))
        (when (or (not (file-exists-p compiled))
                  (file-newer-than-file-p item compiled))
          (byte-compile-file item)
          (setq once t))))
    (if once
        (delete-window-by-name "*Compile-Log*")
      (message "Byte compiled init files exist and are up to date."))))

(defun visit-file (filename)
  "Visit specified file FILENAME.
If the file does not exist, print a message about the fact."
  (let ((filename (expand-file-name filename)))
    (if (file-exists-p filename)
        (find-file filename)
      (message (concat filename " does not exist.")))))

(defmacro ε (fnc &rest args)
  "Interactively invoke function FNC with arguments ARGS.
Kind of partial application."
  `(lambda (&rest rest)
     (interactive)
     (apply ,fnc ,@args rest)))

(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest rest)
     (funcall ,fnc)))

(defun α (input-method dictionary)
  "Switch between input methods and Ispell dictionaries.
Switch between given INPUT-METHOD and DICTIONARY and their defaults."
  (if (eq current-input-method input-method)
      (progn
        (deactivate-input-method)
        (ispell-change-dictionary "default"))
    (set-input-method input-method)
    (ispell-change-dictionary dictionary)))

(defun π (key fnc)
  "Set global key binding that binds KEY to FNC."
  (global-set-key (kbd key) fnc))

(defmacro τ (file keymap key fnc)
  "When FILE is loaded, add to KEYMAP key binding KEY (invoking FNC)."
  `(eval-after-load ',file
     '(define-key
        (symbol-value (intern (concat (symbol-name ',keymap) "-mode-map")))
        (kbd ,key) ,fnc)))

(defmacro λ (&rest args)
  "Return function that interactively return ARGS."
  `(lambda (&rest rest)
     (interactive)
     ',args))

(defvar minor-mode-alias nil
  "Alias for minor modes.")

(defvar major-mode-alias nil
  "Alias for major modes.")

(defun apply-mode-alias ()
  "Use alias from MINOR-MODE-ALIAS and MAJOR-MODE-ALIAS."
  (dolist (x minor-mode-alias)
    (let ((trg (cdr (assoc (car x) minor-mode-alist))))
      (when trg
        (setcar trg (cdr x)))))
  (let ((mode-alias (cdr (assoc major-mode major-mode-alias))))
    (when mode-alias
      (setq mode-name mode-alias))))

(provide 'mk-utils)

;;; mk-utils.el ends here
