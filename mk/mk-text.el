;;; mk-text.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Self-sufficient supplementary text editing commands.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defun mk-column-at (point)
  "Return column number at POINT."
  (save-excursion
    (goto-char point)
    (current-column)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text editing commands

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun mk-mark-command (&optional arg)
  "Set normal mark when ARG is NIL and rectangular otherwise."
  (interactive "P")
  (if arg
      (rectangle-mark-mode 1)
    (set-mark-command nil)))

;;;###autoload
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

;;;###autoload
(defun mk-eat-indent (&optional arg)
  "Delete indentation of current line.

ARG, if given, specifies how many symbols to eat."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (dotimes (_ (or arg 1))
      (when (looking-at "[[:blank:]]")
        (delete-char 1)))))

;;;###autoload
(defun mk-join-lines ()
  "Join the current line with the next line."
  (interactive)
  (delete-indentation t))

;;;###autoload
(defun mk-copy-rest-of-line ()
  "Copy current line from point to end of line."
  (interactive)
  (kill-new (buffer-substring (point) (line-end-position))))

;;;###autoload
(defun mk-copy-buffer ()
  "Put entire buffer into the kill ring."
  (interactive)
  (kill-new (buffer-string)))

;;;###autoload
(defun mk-yank-primary ()
  "Insert contents of the primary selection at the point."
  (interactive)
  (insert (gui-get-selection)))

;;;###autoload
(defun mk-narrow-to-region ()
  "Narrow to region and deactivate the selection."
  (interactive)
  (call-interactively #'narrow-to-region)
  (deactivate-mark))

;;;###autoload
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

;;;###autoload
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

(provide 'mk-text)

;;; mk-text.el ends here
