;;; mk-utils.el --- Various utility functions -*- lexical-binding: t; -*-
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

;; I've collected various auxiliary functions here to avoid cluttering other
;; files.

;;; Code:

(eval-when-compile
  (require 'bookmark)
  (require 'dired)
  (require 'magit))

(require 'avy-menu)
(require 'cl-lib)
(require 'f)
(require 'subr-x)

(defvar mk-dir (f-expand "mk" user-emacs-directory)
  "This is directory where all the configuration files are kept.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text editing

(defun mk-transpose-line-down (&optional arg)
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

(defun mk-transpose-line-up (&optional arg)
  "Move current line and cursor up.

Argument ARG, if supplied, specifies how many times the operation
should be performed."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (let ((col (current-column)))
      (transpose-lines 1)
      (forward-line   -2)
      (move-to-column col))))

(defun mk-duplicate-line (&optional arg)
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

(defun mk-saturated-occurence (&optional after-space)
  "Return position of first non-white space character after point.

If AFTER-SPACE is not NIL, require at least one space character
before target non-white space character."
  (save-excursion
    (let ((this-end (line-end-position)))
      (if (re-search-forward
           (concat (when after-space "[[:blank:]]")
                   "[^[:blank:]]")
           this-end ; don't go after this position
           t)       ; don't error
          (1- (point))
        this-end))))

(defun mk-column-at (point)
  "Return column number at POINT."
  (save-excursion
    (goto-char point)
    (current-column)))

(defun mk-smart-indent (&optional arg)
  "Align first non-white space char after point with content of previous line.

With prefix argument ARG, align to the next line instead."
  (interactive "P")
  (let* ((this-edge (mk-column-at (mk-saturated-occurence)))
         (that-edge
          (save-excursion
            (forward-line (if arg 1 -1))
            (move-to-column this-edge)
            (mk-column-at (mk-saturated-occurence t)))))
    (when (> that-edge this-edge)
      (insert-char 32 (- that-edge this-edge))
      (move-to-column that-edge))))

(defun mk-copy-rest-of-line ()
  "Copy current line from point to end of line."
  (interactive)
  (kill-new (buffer-substring (point) (line-end-position))))

(defun mk-copy-buffer ()
  "Put entire buffer into the kill ring."
  (interactive)
  (kill-new (buffer-string)))

(defun mk-yank-primary ()
  "Insert contents of the primary selection at the point."
  (interactive)
  (insert (gui-get-selection)))

(defun mk-mark-command (&optional arg)
  "Set normal mark when ARG is NIL and rectangular otherwise."
  (interactive "P")
  (if arg
      (rectangle-mark-mode 1)
    (set-mark-command nil)))

(defun mk-narrow-to-region ()
  "Narrow to region and disable the region."
  (interactive)
  (call-interactively #'narrow-to-region)
  (deactivate-mark))

(defun mk-add-to-end-of-lines (beg end text)
  "Append to end of lines between BEG and END given text TEXT.

Interactively, apply it to lines in active region and prompt for
text."
  (interactive "r\nMAdd text: ")
  (save-excursion
    (deactivate-mark)
    (goto-char beg)
    (cl-do ((i 0)
            (total (count-lines beg end)))
        ((= i total))
      (move-end-of-line 1)
      (insert text)
      (forward-line 1)
      (setq i (1+ i)))))

(defun mk-sort-lines-dwim (&optional reverse)
  "Automatically detect and sort block of lines with point in it.

This detects where block of lines with the same indentation
begins and ends and then sorts the entire block.  The block
doesn't not necessarily form a paragraph, sometimes it's just a
part of a paragraph.

When argument REVERSE is not NIL, use descending sort.

When region is active, the command operates within the selected
region between BEG and END."
  (interactive "P")
  (cl-destructuring-bind (beg* . end*)
      (if (region-active-p)
          (cons (point) (mark))
        (save-excursion
          (let* ((origin
                  (progn
                    (back-to-indentation)
                    (point)))
                 (indent (current-column))
                 (beg
                  (progn
                    (while (and (= (current-column) indent)
                                (not (looking-at "^[[:space:]]*$"))
                                (/= (point) (point-min)))
                      (backward-to-indentation 1))
                    (unless (= (point) (point-min))
                      (forward-line 1))
                    (point-at-bol)))
                 (end
                  (progn
                    (goto-char origin)
                    (while (and (= (current-column) indent)
                                (not (looking-at "^[[:space:]]*$"))
                                (/= (point) (point-max)))
                      (forward-to-indentation 1))
                    (point-at-bol))))
            (cons beg end))))
    (let ((origin (point)))
      (sort-lines reverse beg* end*)
      (goto-char origin))))

(defun mk-eat-indentation (&optional arg)
  "Delete indentation of current line.

ARG, if given, specifies how many symbols to eat."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (dotimes (_ (or arg 1))
      (when (looking-at "[[:blank:]]")
        (delete-char 1)))))

(defun mk-single-empty-line ()
  "Make sure we don't have too wide gaps."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]*\n[ \t]*\n\\([ \t]*\n\\)+" nil t)
      (replace-match "\n\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation

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

(defun mk-visit-file (filename)
  "Visit specified file FILENAME.

If the file does not exist, print a message about the fact, but
don't create new empty buffer."
  (let ((filename (f-full filename)))
    (if (f-exists? filename)
        (find-file filename)
      (message "%s does not exist" filename))))

(defun mk-double-buffer ()
  "Show currect buffer in other window and switch to that window."
  (interactive)
  (if (> (length (window-list)) 1)
      (let ((original-buffer (buffer-name)))
        (other-window 1)
        (switch-to-buffer original-buffer))
    (split-window-sensibly)
    (other-window 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Missing commands for the package system

(defun mk-package-upgrade-all ()
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
               (format "Upgrade %d package%s (%s)? "
                       (length upgrades)
                       (if (= (length upgrades) 1) "" "s")
                       (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun mk-shell-quote-arg (arg)
  "Quote ARG for using in the shell.

This function is different from ‘shell-quote-argument’ in that it
can be used for text transformations in Yasnippet without
backslash flood."
  (replace-regexp-in-string "\\W" "\\\\\\&" (remove ?\\ arg)))

(defun mk-anti-ivy-advice (func &rest args)
  "Temporarily disable Ivy and call function FUNC with arguments ARGS."
  (interactive)
  (let ((completing-read-function #'completing-read-default))
    (if (called-interactively-p 'any)
        (call-interactively func)
      (apply func args))))

(defun mk-disable-ivy (command)
  "Disable Ivy when command COMMAND is called."
  (advice-add command :around #'mk-anti-ivy-advice))

(defun mk-set-sentence-end-double-space ()
  "Set ‘sentence-end-double-space’ to T locally."
  (setq-local sentence-end-double-space t))

(defun mk-use-lang (input-method dictionary)
  "Switch between input methods and Ispell dictionaries.

Switch between given INPUT-METHOD and DICTIONARY and their
defaults."
  (if (eq current-input-method input-method)
      (progn
        (deactivate-input-method)
        (ispell-change-dictionary "default"))
    (set-input-method input-method)
    (ispell-change-dictionary dictionary)))

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

(defmacro mk-translate-kbd (from to)
  "Translate combinations of keys FROM to TO combination.

Effect of this translation is global."
  `(define-key key-translation-map (kbd ,from) (kbd ,to)))

(defun mk-find-file (regexp)
  "Find file whose name satisfies REGEXP traversing upwards.

Return absolute path to directory containing that file or NIL on
failure."
  (let ((dir (f-traverse-upwards
              (lambda (path)
                (directory-files path t regexp t))
              (f-full default-directory))))
    (when dir
      (f-slash dir))))

(defmacro mk-with-directory-of-file (regexp &rest body)
  "Find file with name matching REGEXP and operate in its directory.

Searching for file is performed with ‘mk-find-file’.  If the
function returns NIL, don't execute BODY.  Otherwise temporarily
bind ‘default-directory’ to directory of found file and execute
BODY."
  (declare (indent defun))
  (let ((dir (cl-gensym)))
    `(let ((,dir (mk-find-file ,regexp)))
       (if ,dir
           (let ((default-directory ,dir))
             ,@body)
         (message ,(format "Cannot find file matching %s" regexp))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility commands

(defun mk-bookmark-jump (&optional arg)
  "Jump to a bookmark.

By default open new location in the left window, but when ARG is
given open it in the right window.

Any-menu is used to select bookmark."
  (interactive "P")
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (unless bookmark-alist
    (error "You better create some bookmarks first"))
  (let ((bookmark
         (avy-menu
          "*bookmarks*"
          (list "Jump to Bookmark"
                (cons "Pane"
                      (mapcar (lambda (x) (cons (car x) x))
                              bookmark-alist)))))
        (in-first-window
         (eq (frame-first-window)
             (selected-window))))
    (when bookmark
      (bookmark-jump
       bookmark
       (if (if in-first-window (not arg) arg)
           #'switch-to-buffer
         #'switch-to-buffer-other-window)))))

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

(defun mk-show-date (&optional stamp)
  "Show current date in the minibuffer.

If STAMP is not NIL, insert date at point."
  (interactive)
  (funcall (if stamp #'insert #'message)
           (format-time-string "%A, %e %B, %Y")))

(defun mk-show-default-dir ()
  "Show default directory in the minibuffer."
  (interactive)
  (message (f-full default-directory)))

(defun mk-file-name-to-kill-ring (arg)
  "Put name of file into kill ring.

If user's visiting a buffer that's associated with a file, use
name of the file.  If major mode is ‘dired-mode’, use name of
file at point, but if point is not placed at any file, put name
of actual directory into kill ring.  Argument ARG, if given,
makes result string be quoted as for yanking into shell."
  (interactive "P")
  (let ((φ (if (cl-find major-mode
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

(defun mk-melpa-page (package)
  "Go to the MELPA page of PACKAGE."
  (interactive
   (list
    (completing-read "MELPA: "
                     (mapcar #'car package-archive-contents))))
  (browse-url
   (concat "https://melpa.org/#/"
           (url-hexify-string package))))

(defun mk-package-page (package)
  "Go to the PACKAGE home page if it exists."
  (interactive
   (list
    (intern
     (completing-read "Package's home page: "
                      (mapcar #'car package-archive-contents)))))
  (message "%s "package)
  (let ((home-page
         (cdr
          (assq :url
                (package-desc-extras
                 (cadr (assq package package-archive-contents)))))))
    (when home-page
      (browse-url home-page))))

(defun mk-compile-init-files ()
  "Byte compile init files (all *.el files under ‘mk-dir’ directory)."
  (interactive)
  (let (once)
    (save-window-excursion
      (dolist (item (cons (f-full user-init-file)
                          (directory-files mk-dir t "\\`[^#].*\\.el\\'" t)))
        (let ((compiled (byte-compile-dest-file item)))
          (when (or (not (f-file? compiled))
                    (file-newer-than-file-p item compiled))
            (byte-compile-file item)
            (setq once t)))))
    (unless once
      (message "Byte compiled init files exist and are up to date"))))

(defun mk-eval-last-sexp ()
  "Evaluate last S-expression and replace it with the result."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun mk-exit-emacs (&optional arg)
  "Exit Emacs: save all file-visiting buffers, kill terminal.

If ARG is given and it's not NIL, don't ask user if he wants to
exit."
  (interactive "P")
  (when (or arg (yes-or-no-p "Exit Emacs?"))
    (save-buffers-kill-terminal)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My little helpers from Greece…

(defmacro σ (&rest args)
  "Return function that returns list of ARGS."
  `(lambda (&rest _rest)
     (list ,@args)))

(defmacro ε (fnc &rest args)
  "Interactively invoke function FNC with arguments ARGS."
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
