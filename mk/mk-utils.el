;;; mk-utils.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; I've collected various auxiliary functions here to avoid cluttering other
;; files.

;;; Code:

(eval-when-compile
  (require 'dired))

(require 'cl-lib)
(require 'f)
(require 'subr-x)

(defvar mk-dir (f-expand "mk" user-emacs-directory)
  "This is directory where all the configuration files are kept.")

(defmacro mk-translate-kbd (from to)
  "Translate combinations of keys FROM to TO combination."
  `(define-key key-translation-map (kbd ,from) (kbd ,to)))

(defun mk-set-key (key fnc)
  "Set global key binding that binds KEY to FNC."
  (global-set-key (kbd key) fnc))

(defmacro mk-iwrap (fnc &rest args)
  "Interactively invoke function FNC with arguments ARGS."
  `(lambda (&rest rest)
     (interactive)
     (apply ,fnc ,@args rest)))

(defun mk-visit-file (filename)
  "Visit specified file FILENAME.

If the file does not exist, print a message about the fact, but
don't create new empty buffer."
  (let ((filename (f-full filename)))
    (if (f-exists? filename)
        (find-file filename)
      (message "%s does not exist" filename))))

(defun mk-shell-quote-arg (arg)
  "Quote ARG for using in the shell.

This function is different from ‘shell-quote-argument’ in that it
can be used for text transformations in Yasnippet without
backslash flood."
  (replace-regexp-in-string "\\W" "\\\\\\&" (remove ?\\ arg)))

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

(defun mk-get-existing-projects (dir)
  "Return a list of existing projects under DIR.

All projects are combinations of two directory segments (org or
user, then project name, slash separated) immediately under the
specified directory."
  (cl-sort
   (mapcar (lambda (path)
             (f-relative path "~/projects"))
           (f-glob "~/projects/*/*"))
   #'string-lessp))

(defun mk-project-jump (project-name)
  "Jump to PROJECT-NAME opening it in Dired."
  (interactive
   (list
    (completing-read "Projects: "
                     (mk-get-existing-projects "~/projects"))))
  (find-file
   (f-expand project-name "~/projects")))

(defun mk-show-date (&optional stamp)
  "Show current date in the minibuffer.

If STAMP is not NIL, insert date at point."
  (interactive)
  (funcall (if stamp #'insert #'message)
           (format-time-string "%A, %e %B, %Y")))

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

(defun mk-compile-init-files ()
  "Byte compile init files (all *.el files under ‘mk-dir’ directory)."
  (interactive)
  (let (once)
    (save-window-excursion
      ;; TODO this often includes flycheck temporary files.  Avoid that.
      (dolist (item (cons (f-full user-init-file)
                          (directory-files mk-dir t "\\`[^#].*\\.el\\'" t)))
        (let ((compiled (byte-compile-dest-file item)))
          (when (or (not (f-file? compiled))
                    (file-newer-than-file-p item compiled))
            (byte-compile-file item)
            (setq once t)))))
    (unless once
      (message "Byte compiled init files exist and are up to date"))))

(provide 'mk-utils)

;;; mk-utils.el ends here
