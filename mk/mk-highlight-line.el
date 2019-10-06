;;; mk-highlight-line.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs has a minor mode for highlighting current line: ‘hl-line-mode’.
;; It's worth enabling in many types of buffers that deal with lists of
;; items, one item per line.  This package allows us to enable it everywhere
;; for readability.

;;; Code:

(defvar mk-highlight-line-target-modes
  '(Buffer-menu-mode
    bookmark-bmenu-mode
    dired-mode
    gnus-group-mode
    gnus-summary-mode
    ibuffer-mode
    package-menu-mode)
  "List of modes that are affected by ‘mk-highlight-line-mode’.")

;;;###autoload
(define-minor-mode mk-highlight-line-mode
  "Toggle highlight-line-mode minor mode.

With a prefix argument ARG, enable highlight-line if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
‘toggle’.

This minor mode is global. When it's active, major modes listed
in ‘mk-highlight-line-target-modes’ activate ‘hl-line-mode’
automatically."
  :global t
  (dolist (mode mk-highlight-line-target-modes)
    (funcall
     (if mk-highlight-line-mode
         #'add-hook
       #'remove-hook)
     (intern (concat (symbol-name mode) "-hook"))
     #'hl-line-mode)))

(provide 'mk-highlight-line)

;;; mk-highlight-line.el ends here
