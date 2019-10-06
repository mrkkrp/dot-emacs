;;; mk-packages.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Per-package configurations.

;;; Code:

(require 'mk-utils)
(require 'use-package)

(defvar mk-avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)
  "Home row Dvorak keys.")

(use-package ace-link
  :commands (ace-link ace-link-org)
  :init
  (setq-default ace-link-fallback-function 'ace-link-org)
  :config
  (ace-link-setup-default)
  :bind
  ("M-o" . ace-link))

(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode 1))

(use-package ace-window
  :commands (ace-window)
  :init
  (setq-default
   aw-keys mk-avy-keys
   aw-background nil)
  :bind
  ("<prior>" . ace-window))

(use-package aggressive-indent
  :bind
  ("<next> a g" . aggressive-indent-mode)
  :hook
  ((emacs-lisp-mode . aggressive-indent-mode)
   (html-mode . aggressive-indent-mode)))

(use-package align
  :bind
  ("<next> a r" . align-regexp))

(use-package autorevert
  :init
  (setq-default
   auto-revert-verbose nil
   global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

(use-package browse-url
  :init
  (setq
   browse-url-browser-function 'browse-url-generic
   browse-url-generic-program "google-chrome-stable"))

(use-package calc
  :commands (calc)
  :bind
  ("<next> c a" . calc))

(use-package calendar
  :commands (calendar)
  :init
  (setq calendar-week-start-day 1)
  :bind
  (("<next> c l" . calendar)
   :map
   calendar-mode-map
   ("<prior>" . ace-window)))

(use-package cus-edit
  :bind
  ("<next> c g" . customize-group))

(use-package avy
  :after (modalka)
  :commands (avy-goto-line)
  :init
  (setq-default
   avy-keys mk-avy-keys
   avy-style 'at-full)
  :bind
  (:map
   modalka-mode-map
   ("J" . avy-goto-line)))

(use-package char-menu
  :commands (char-menu)
  :init
  (setq-default
   char-menu
   '("—" "‘’" "“”" "…" "«»"
     ("Typography"
      "–" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
     ("Math"
      "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√" "∇")
     ("Arrows"
      "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
     ("Greek"
      "α" "β" "Δ" "δ" "ε" "ζ" "η" "θ" "λ" "μ" "ν" "ξ"
      "Ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω" "Ω")))
  :bind
  ("<next> DEL" . char-menu))

(use-package counsel
  :commands (counsel-M-x)
  :bind
  ("M-x" . counsel-M-x))

(use-package custom
  :config
  (defun mk-switch-theme (theme)
    "Switch to theme THEME, loading it if necessary."
    (interactive
     (list
      (intern
       (completing-read "Switch to theme: "
                        (mapcar #'symbol-name
                                (custom-available-themes))))))
    (dolist (enabled-theme custom-enabled-themes)
      (disable-theme enabled-theme))
    (load-theme theme t))
  :bind
  ("<next> t h" . mk-switch-theme))

(use-package cyphejor
  :init
  (setq
   cyphejor-rules
   '(:upcase
     ("bookmark"    "→")
     ("buffer"      "β")
     ("diff"        "Δ")
     ("dired"       "δ")
     ("emacs"       "ε")
     ("eshell"      "εsh")
     ("inferior"    "i" :prefix)
     ("interaction" "i" :prefix)
     ("interactive" "i" :prefix)
     ("lisp"        "λ" :postfix)
     ("menu"        "▤" :postfix)
     ("mode"        "")
     ("package"     "↓")
     ("python"      "π")
     ("shell"       "sh" :postfix)
     ("text"        "ξ")
     ("wdired"      "↯δ")))
  :config
  (cyphejor-mode 1))

(use-package dabbrev
  :commands (dabbrev-expand)
  :bind
  ("C-," . dabbrev-expand))

(use-package delsel
  :demand
  :config
  (delete-selection-mode 1))

(use-package descr-text
  :bind
  ("<next> d c" . describe-char))

(use-package dired
  :init
  (setq
   delete-by-moving-to-trash t
   dired-auto-revert-buffer t
   dired-dwim-target t
   dired-keep-marker-copy nil
   dired-listing-switches "-GAlh --group-directories-first"
   dired-recursive-copies 'always
   dired-recursive-deletes 'always)
  :config

  (defun mk-dired-first-file ()
    "Jump to the first file in current directory."
    (interactive)
    (goto-char (point-min))
    (dired-next-line 2))

  (defun mk-dired-last-file ()
    "Jump to the last file in current directory."
    (interactive)
    (goto-char (point-max))
    (dired-previous-line 1))

  (defun mk-dired-open-external (file)
    "Open specified FILE with application determined by the OS."
    (interactive (list (dired-get-filename)))
    (call-process "xdg-open" nil 0 nil file))

  :bind
  (:map
   dired-mode-map
   ("<down>" . mk-dired-last-file)
   ("<up>" . mk-dired-first-file)
   ("b" . dired-up-directory)
   ("e" . mk-dired-open-external)
   ("w" . wdired-change-to-wdired-mode))
  :hook
  ((dired-mode . toggle-truncate-lines)))

(use-package dired-x
  :init
  (setq
   dired-clean-up-buffers-too t))

(use-package direnv
  :config
  (direnv-mode 1))

(use-package eldoc
  :init
  (setq eldoc-idle-delay 0.1)
  :hook
  ((emacs-lisp-mode . eldoc-mode)))

(use-package electric
  :demand
  :config
  (electric-indent-mode 0))

(use-package elisp-mode
  :config
  (defun mk-eval-last-sexp ()
    "Evaluate last S-expression and replace it with the result."
    (interactive)
    (let ((value (eval (elisp--preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))
  (defun mk-set-sentence-end-double-space ()
    "Set ‘sentence-end-double-space’ to T locally."
    (setq-local sentence-end-double-space t))
  :bind
  ("<next> e d" . eval-defun)
  ("<next> e e" . eval-last-sexp)
  ("<next> e v" . eval-buffer)
  ("M-e" . mk-eval-last-sexp)
  :hook
  ((emacs-lisp-mode . mk-set-sentence-end-double-space)
   (lisp-interaction-mode . eldoc-mode)))

(use-package files
  :demand
  :init
  (setq
   auto-save-default nil
   backup-by-copying t
   backup-directory-alist (list (cons "." (f-expand "backups" user-emacs-directory)))
   delete-old-versions t
   kept-new-versions 4
   kept-old-versions 2
   large-file-warning-threshold 10240000
   make-backup-files t
   require-final-newline t
   vc-display-status nil
   version-control t)
  :config

  (defun mk-single-empty-line ()
    "Make sure we don't have too wide gaps."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*\n[ \t]*\n\\([ \t]*\n\\)+" nil t)
        (replace-match "\n\n"))))

  (defun mk-exit-emacs (&optional arg)
    "Exit Emacs: save all file-visiting buffers, kill terminal.

If ARG is given and it's not NIL, don't ask user if he wants to
exit."
    (interactive "P")
    (when (or arg (yes-or-no-p "Exit Emacs?"))
      (save-buffers-kill-emacs)))

  (advice-add 'revert-buffer :filter-args (lambda (&rest _rest) (list nil t)))

  :bind
  ("<f12>" . mk-exit-emacs)
  ("<next> a s" . write-file)
  ("<next> r r" . revert-buffer)
  :hook
  ((before-save . whitespace-cleanup)
   (before-save . mk-single-empty-line)))

(use-package find-func
  :bind
  ("<next> f f" . find-function)
  ("<next> f v" . find-variable))

(use-package fix-input
  :demand
  :config
  (fix-input "english-dvorak" "russian-computer" "mk-dvorak-russian"))

(use-package fix-word
  :commands (fix-word-capitalize fix-word-downcase fix-word-upcase)
  :bind
  ("M-c" . fix-word-capitalize)
  ("M-l" . fix-word-downcase)
  ("M-u" . fix-word-upcase))

(use-package flycheck
  :config
  (setq-default
   flycheck-emacs-lisp-initialize-packages t
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-temp-prefix ".flycheck"
   flycheck-disabled-checkers '(haskell-stack-ghc
                                haskell-ghc
                                haskell-hlint))
  :bind
  ("<next> f l" . flycheck-list-errors)
  :hook
  ((gitignore-mode . flycheck-mode)
   (markdown-mode . flycheck-mode)
   (prog-mode . flycheck-mode)
   (proof-mode . flycheck-mode)
   (yaml-mode . flycheck-mode))
  :custom-face
  (flycheck-fringe-error ((t (:background "#6C3333" :weight bold)))))

(use-package flycheck-color-mode-line
  :after (flycheck)
  :hook
  ((flycheck-mode . flycheck-color-mode-line-mode)))

(use-package flycheck-mmark
  :after (flycheck)
  :hook
  ((flycheck-mode . flycheck-mmark-setup)))

(use-package flyspell
  :config
  (defun mk-flyspell-correct-previous (&optional words)
    "Correct word before point, reach distant words.

WORDS words at maximum are traversed backward until misspelled
word is found.  If it's not found, give up.  If argument WORDS is
not specified, traverse 12 words by default.

Return T if misspelled word is found and NIL otherwise.  Never
move point."
    (interactive "P")
    (let* ((Δ (- (point-max) (point)))
           (counter (string-to-number (or words "12")))
           (result
            (catch 'result
              (while (>= counter 0)
                (when (cl-some #'flyspell-overlay-p
                               (overlays-at (point)))
                  (flyspell-correct-word-before-point)
                  (throw 'result t))
                (backward-word 1)
                (setq counter (1- counter))
                nil))))
      (goto-char (- (point-max) Δ))
      result))
  :bind
  (:map
   flyspell-mode-map
   (("C-," . nil)
    ("C-." . nil)
    ("C-;" . mk-flyspell-correct-previous)))
  :hook
  ((gitignore-mode . flyspell-prog-mode)
   (haskell-cabal-mode . flyspell-prog-mode)
   (prog-mode . flyspell-prog-mode)
   (proof-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode)
   (yaml-mode . flyspell-prog-mode)))

(use-package flyspell-lazy
  :after (flyspell)
  :init
  (setq-default
   flyspell-lazy-disallow-buffers nil
   flyspell-lazy-idle-seconds 1)
  :config
  (flyspell-lazy-mode 1))

(use-package frame
  :config

  (blink-cursor-mode 0)

  (defun mk-set-font (font &optional height)
    "Set font FONT as main font for all frames.

HEIGHT, if supplied, specifies height of letters to use."
    (interactive
     (list (completing-read "Use font: " (font-family-list)) nil))
    (set-face-attribute 'default nil :family font)
    (when height
      (set-face-attribute 'default nil :height height))
    (set-face-attribute 'variable-pitch nil :family font))

  (when window-system
    (mk-set-font "DejaVu Sans Mono" 120)
    (toggle-frame-fullscreen))

  :bind
  ("<next> d f" . delete-frame)
  ("<next> f o" . mk-set-font)
  ("<next> n f" . make-frame)
  ("<next> t f" . toggle-frame-fullscreen))

(use-package git-link
  :commands (git-link)
  :bind
  ("<next> g g" . git-link))

(use-package grep
  :commands (rgrep)
  :bind
  ("<next> g r" . rgrep))

(use-package ispell
  :after (fix-input)
  :init
  (setq-default
   ispell-dictionary "en")
  :config
  (defun mk-use-lang (input-method dictionary)
    "Switch to INPUT-METHOD and Ispell DICTIONARY."
    (set-input-method input-method)
    (ispell-change-dictionary dictionary))
  (defun mk-use-en ()
    "Switch to English."
    (interactive)
    (mk-use-lang nil "english"))
  (defun mk-use-fr ()
    "Switch to French."
    (interactive)
    (mk-use-lang nil "french"))
  (defun mk-use-ru ()
    "Switch to Russian."
    (interactive)
    (mk-use-lang "mk-dvorak-russian" "ru"))
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  :bind
  ("<next> e n" . mk-use-en)
  ("<next> f r" . mk-use-fr)
  ("<next> r u" . mk-use-ru))

(use-package haskell-cabal
  :bind
  (:map
   haskell-cabal-mode-map
   ("M-n" . mk-transpose-line-down)
   ("M-p" . mk-transpose-line-up)))

(use-package haskell-interactive-mode
  :bind
  (:map
   haskell-interactive-mode-map
   ("<end>" . nil)
   ("<escape>" . nil)
   ("<home>" . nil)
   ("<next>" . nil)
   ("<prior>" . nil)
   ("C-<prior>" . nil)
   ("C-c r" . haskell-process-restart)))

(use-package haskell-mode
  :init
  (setq
   haskell-ask-also-kill-buffers nil
   haskell-process-load-or-reload-prompt t
   haskell-process-show-debug-tips nil
   haskell-process-type 'stack-ghci
   haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))
  :config
  (defun mk-haskell-insert-symbol ()
    "Insert one of the Haskell symbols that are difficult to type."
    (interactive)
    (char-menu
     '("<-" "::"  "->"  "=>"  "="
       "<*" "<$>" "<*>" "<|>" "*>")))
  :bind
  (("<next> o" . mk-haskell-insert-symbol)
   :map
   haskell-indentation-mode-map
   ("RET" . nil)
   ("<backtab>" . nil)
   ("," . nil)
   (";" . nil)
   (")" . nil)
   ("}" . nil)
   ("]" . nil)
   :map
   haskell-mode-map
   ("M-." . haskell-mode-jump-to-def)
   ("C-c C-u" . haskell-mode-generate-tags)
   ("M-," . pop-tag-mark))
  :hook
  ((haskell-mode . interactive-haskell-mode)))

(use-package hasky-extensions
  :after (haskell-mode)
  :commands (hasky-extensions)
  :bind
  (:map
   haskell-mode-map
   ("C-c y" . hasky-extensions))
  :hook
  ((hasky-extensions-prettifying . mk-single-empty-line)
   (hasky-extensions-prettifying . whitespace-cleanup)))

(use-package hasky-stack
  :commands (hasky-stack-execute hasky-stack-package-action hasky-stack-new)
  :init
  (setq
   hasky-stack-auto-target t
   hasky-stack-auto-open-coverage-reports t
   hasky-stack-auto-newest-version t)
  :bind
  ("<next> h e" . hasky-stack-execute)
  ("<next> h o" . hasky-stack-package-action)
  ("<next> h i" . hasky-stack-new))

(use-package help
  :bind
  ("<next> f k" . describe-key))

(use-package highlight-symbol
  :commands
  (highlight-symbol-remove-all
   highlight-symbol-next
   highlight-symbol-prev
   highlight-symbol)
  :bind
  ("<next> h a" . highlight-symbol-remove-all)
  ("<next> h n" . highlight-symbol-next)
  ("<next> h p" . highlight-symbol-prev)
  ("<next> h s" . highlight-symbol))

(use-package hl-todo
  :hook
  ((gitignore-mode . hl-todo-mode)
   (haskell-cabal-mode . hl-todo-mode)
   (prog-mode . hl-todo-mode)
   (proof-mode . hl-todo-mode)
   (yaml-mode . hl-todo-mode)))

(use-package ivy
  :init
  (setq-default
   ivy-use-selectable-prompt t)
  :config
  (defun mk-anti-ivy-advice (func &rest args)
    "Temporarily disable Ivy and call function FUNC with arguments ARGS."
    (interactive)
    (let ((completing-read-function #'completing-read-default))
      (if (called-interactively-p 'any)
          (call-interactively func)
        (apply func args))))
  (advice-add 'dired-create-directory :around 'mk-anti-ivy-advice)
  (dolist (buffer '("^\\*Backtrace\\*$"
                    "^\\*Compile-Log\\*$"
                    "^\\*.+Completions\\*$"
                    "^\\*Flycheck error messages\\*$"
                    "^\\*Help\\*$"
                    "^\\*Ibuffer\\*$"
                    "^\\*Messages\\*$"
                    "^\\*inferior-lisp\\*$"
                    "^\\*scratch\\*$"))
    (add-to-list 'ivy-ignore-buffers buffer))
  (ivy-mode 1))

(use-package js2-mode
  :after (mk-text)
  :mode "\\.js$"
  :bind
  (:map
   js2-mode-map
   ("M-j" . mk-join-lines)))

(use-package kill-or-bury-alive
  :commands (kill-or-bury-alive kill-or-bury-alive-purge-buffers)
  :bind
  ("<escape>" . kill-or-bury-alive)
  ("<next> a a" . kill-or-bury-alive-purge-buffers))

(use-package latex
  :config

  (defun mk-pdf-latex-generate (&optional output-dir)
    "Generate PDF document from currently opened LaTeX document.

If given, OUTPUT-DIR specifies directory where all the temporary
and result files a are stored.  If it's not given, new directory
with unique name is used instead.  Return name of PDF file
produced."
    (let* ((output-dir (or output-dir (make-temp-file "pdflatex" t)))
           (buffer-file-name (buffer-file-name))
           (output-pdf (f-join output-dir
                               (f-swap-ext (f-base buffer-file-name)
                                           "pdf"))))
      (if (not buffer-file-name)
          (error "Must be visiting a file")
        (save-window-excursion
          (shell-command
           (concat "pdflatex -output-directory "
                   (shell-quote-argument output-dir)
                   " "
                   (shell-quote-argument buffer-file-name))))
        (message "Written %s" output-pdf)
        output-pdf)))

  (defun mk-pdf-latex-preview ()
    "Generate temporary PDF file and open it with external application."
    (interactive)
    (call-process "xdg-open" nil 0 nil (mk-pdf-latex-generate)))

  (defun mk-pdf-latex-export ()
    "Create PDF document based on current TeX/LaTeX file."
    (interactive)
    (mk-pdf-latex-generate default-directory))

  (advice-add 'TeX-insert-quote :filter-args (lambda (&rest _rest) (list t)))

  :bind
  (:map
   LaTeX-mode-map
   ("C-c C-l" . mk-pdf-latex-export)
   ("C-c C-v" . mk-pdf-latex-preview)))

(use-package magit
  :init
  (setq magit-clone-set-remote.pushDefault t)
  :bind
  (("<next> m c" . magit-clone)
   ("<next> m i" . magit-init)
   ("<next> m s" . magit-status)
   :map
   git-commit-mode-map
   ("M-n" . mk-transpose-line-down)
   ("M-p" . mk-transpose-line-up)))

(use-package markdown-mode
  :init
  (setq markdown-url-compose-char ?…)
  :bind
  (("<next> m d" . markdown-mode)
   :map
   markdown-mode-map
   ("<return>" . nil)
   ("M-n" . mk-transpose-line-down)
   ("M-p" . mk-transpose-line-up)))

(use-package merlin
  :hook
  ((tuareg-mode . merlin-mode)))

(use-package minibuf-eldef
  :demand
  :init
  (setq
   minibuffer-eldef-shorten-default t)
  :config
  (minibuffer-electric-default-mode 1))

(use-package modalka
  :after (mk-text mk-utils)
  :init
  (setq-default
   modalka-cursor-type 'box)
  :config
  (modalka-define-kbd "SPC" "C-SPC")
  ;; ' (handy as self-inserting)
  ;; " (handy as self-inserting)
  (modalka-define-kbd "," "C-,")
  ;; - (handy as self-inserting)
  (modalka-define-kbd "/" "M-.")
  (modalka-define-kbd "." "C-.")
  (modalka-define-kbd ":" "M-;")
  (modalka-define-kbd ";" "C-;")
  (modalka-define-kbd "?" "M-,")

  (modalka-define-kbd "0" "C-0")
  (modalka-define-kbd "1" "C-1")
  (modalka-define-kbd "2" "C-2")
  (modalka-define-kbd "3" "C-3")
  (modalka-define-kbd "4" "C-4")
  (modalka-define-kbd "5" "C-5")
  (modalka-define-kbd "6" "C-6")
  (modalka-define-kbd "7" "C-7")
  (modalka-define-kbd "8" "C-8")
  (modalka-define-kbd "9" "C-9")

  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "b" "C-b")
  (modalka-define-kbd "c b" "C-c C-b")
  (modalka-define-kbd "c c" "C-c C-c")
  (modalka-define-kbd "c k" "C-c C-k")
  (modalka-define-kbd "c l" "C-c C-l")
  (modalka-define-kbd "c n" "C-c C-n")
  (modalka-define-kbd "c s" "C-c C-s")
  (modalka-define-kbd "c t" "C-c C-t")
  (modalka-define-kbd "c u" "C-c C-u")
  (modalka-define-kbd "c v" "C-c C-v")
  (modalka-define-kbd "c x" "C-c C-x")
  (modalka-define-kbd "d" "C-d")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "f" "C-f")
  (modalka-define-kbd "g" "C-g")
  (modalka-define-kbd "h" "M-h")
  (modalka-define-kbd "i" "C-i")
  (modalka-define-kbd "j" "M-j")
  (modalka-define-kbd "k" "C-k")
  (modalka-define-kbd "l" "C-l")
  (modalka-define-kbd "m" "C-m")
  (modalka-define-kbd "n" "C-n")
  (modalka-define-kbd "o" "C-o")
  (modalka-define-kbd "p" "C-p")
  (modalka-define-kbd "q" "M-q")
  (modalka-define-kbd "r" "C-r")
  (modalka-define-kbd "s" "C-s")
  (modalka-define-kbd "t" "C-t")
  (modalka-define-kbd "u" "C-u")
  (modalka-define-kbd "v" "C-v")
  (modalka-define-kbd "w" "C-w")
  (modalka-define-kbd "x 3" "C-x #")
  (modalka-define-kbd "x ;" "C-x C-;")
  (modalka-define-kbd "x e" "C-x C-e")
  (modalka-define-kbd "x o" "C-x C-o")
  (modalka-define-kbd "y" "C-y")
  (modalka-define-kbd "z" "M-z")

  (modalka-define-kbd "A" "M-SPC")
  (modalka-define-kbd "B" "M-b")
  (modalka-define-kbd "C" "M-c")
  (modalka-define-kbd "D" "M-d")
  (modalka-define-kbd "E" "M-e")
  (modalka-define-kbd "F" "M-f")
  (modalka-define-kbd "G" "C-`")
  (modalka-define-kbd "H" "M-H")
  ;; I (bound elsewhere)
  ;; J (bound elsewhere)
  (modalka-define-kbd "K" "M-k")
  (modalka-define-kbd "L" "M-l")
  (modalka-define-kbd "M" "M-m")
  (modalka-define-kbd "N" "M-n")
  (modalka-define-kbd "O" "M-o")
  (modalka-define-kbd "P" "M-p")
  ;; Q (bound elsewhere)
  (modalka-define-kbd "R" "M-r")
  (modalka-define-kbd "S" "M-S")
  (modalka-define-kbd "T" "M-t")
  (modalka-define-kbd "U" "M-u")
  (modalka-define-kbd "V" "M-v")
  (modalka-define-kbd "W" "M-w")
  ;; X (not bound)
  (modalka-define-kbd "Y" "M-y")
  (modalka-define-kbd "Z" "C-z")

  (defun mk-modalka-mode-no-git-commit ()
    "Enable ‘modalka-mode’ unless get edit git commit message."
    (unless (string-equal (buffer-name) "COMMIT_EDITMSG")
      (modalka-mode 1)))

  (defun mk-open-default-dir ()
    "Open default directory."
    (interactive)
    (find-file default-directory))

  :bind
  (("<return>" . modalka-mode)
   :map
   modalka-mode-map
   ("Q" . mk-sort-lines-dwim)
   ("X" . mk-open-default-dir))
  :hook
  ((compilation-mode . modalka-mode)
   (conf-toml-mode . modalka-mode)
   (conf-unix-mode . modalka-mode)
   (gitignore-mode . modalka-mode)
   (haskell-cabal-mode . modalka-mode)
   (help-mode . modalka-mode)
   (info-mode . modalka-mode)
   (mustache-mode . modalka-mode)
   (prog-mode . modalka-mode)
   (proof-mode . modalka-mode)
   (text-mode . mk-modalka-mode-no-git-commit)
   (yaml-mode . modalka-mode)
   (ztree-mode . modalka-mode)))

(use-package mk-highlight-line
  :demand
  :config
  (mk-highlight-line-mode 1))

(use-package mk-text
  :demand
  :commands
  (mk-transpose-line-down
   mk-transpose-line-up
   mk-duplicate-line
   mk-mark-command
   mk-smart-indent
   mk-eat-indent
   mk-join-lines
   mk-copy-rest-of-line
   mk-copy-buffer
   mk-yark-primary
   mk-narrow-to-region
   mk-add-to-end-of-lines
   mk-sort-lines-dwim)
  :bind
  ("M-n" . mk-transpose-line-down)
  ("M-p" . mk-transpose-line-up)
  ("M-r" . mk-duplicate-line)
  ("C-SPC" . mk-mark-command)
  ("C-r" . mk-smart-indent)
  ("M-j" . mk-join-lines)
  ("C-z" . mk-copy-rest-of-line)
  ("<next> e l" . mk-add-to-end-of-lines)
  ("<next> n n" . mk-narrow-to-region)
  ("<next> n w" . widen)
  ("<next> y p" . mk-yank-primary))

(use-package mule
  :bind
  ("<next> c c" . revert-buffer-with-coding-system)
  ("<next> c s" . set-buffer-file-coding-system))

(use-package mwheel
  :demand
  :config
  (mouse-wheel-mode 0))

(use-package nix-mode
  :mode "\\.nix$"
  :config

  (defun mk-nixos-package (symbol)
    "Find information about a NixOS package given SYMBOL."
    (interactive (list (mk-grab-input "NixOS package: ")))
    (browse-url
     (concat "https://nixos.org/nixos/packages.html?query="
             (url-hexify-string symbol))))

  (defun mk-nixos-option (symbol)
    "Find information abouta NixOS option given SYMBOL."
    (interactive (list (mk-grab-input "NixOS option: ")))
    (browse-url
     (concat "https://nixos.org/nixos/options.html#"
             (url-hexify-string symbol))))

  :bind
  ("<next> n o" . mk-nixos-option)
  ("<next> n p" . mk-nixos-package))

(use-package package
  :config
  (advice-add 'package-install :filter-args (lambda (args) (list (car args) t)))
  (use-package mk-package)
  :bind
  ("<next> l p" . list-packages)
  ("<next> m e" . mk-melpa-page)
  ("<next> p a" . package-autoremove)
  ("<next> p f" . package-install-file)
  ("<next> p i" . package-install)
  ("<next> p p" . mk-package-page)
  ("<next> p u" . mk-package-upgrade-all))

(use-package paragraphs
  :bind
  ("M-H" . mark-paragraph))

(use-package paren
  :init
  (setq
   show-paren-delay 0.05)
  :config
  (show-paren-mode 1))

(use-package proof-general
  :after (proof-script proof-useropts)
  :init
  (setq
   proof-splash-enable nil
   proof-three-window-enable nil)
  :bind
  (:map
   proof-mode-map
   ("C-c C-s" . proof-goto-point)
   ("M-n" . mk-transpose-line-down)
   ("M-p" . mk-transpose-line-up)))

(use-package python
  :commands (python-mode)
  :mode
  ("\\.bzl$" . python-mode)
  ("BUILD$" . python-mode)
  ("WORKSPACE$" . python-mode)
  :init
  (setq-default
   python-fill-docstring-style 'pep-257-nn
   python-indent-offset 2)
  :bind
  ("<next> p y" . python-mode))

(use-package rainbow-delimiters
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package rect
  :bind
  ("<next> c r" . copy-rectangle-as-kill)
  ("<next> k r" . kill-rectangle)
  ("<next> s r" . string-rectangle)
  ("<next> y r" . yank-rectangle))

(use-package register
  :bind
  ("<next> r c" . copy-to-register)
  ("<next> r i" . insert-register))

(use-package replace
  :bind
  ("<next> q r" . query-replace))

(use-package rich-minority
  :init
  (setq
   rm-whitelist "^↑$"
   rm-text-properties '(("^↑$" 'face 'font-lock-doc-face)))
  :config
  (rich-minority-mode 1))

(use-package scroll-bar
  :demand
  :config
  (scroll-bar-mode 0))

(use-package server
  :demand
  :config
  (unless (server-running-p)
    (server-start)))

(use-package sgml-mode
  :mode
  ("\\.tpl$" . html-mode)
  ("\\.mustache$" . html-mode))

(use-package simple
  :demand
  :init
  (setq
   blink-matching-delay 0.5
   blink-matching-paren 'jump-offscreen
   kill-read-only-ok t
   suggest-key-bindings nil)
  :config
  (defun mk-auto-fill-mode ()
    "Enable ‘auto-fill-mode’ limiting it to comments."
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))
  (column-number-mode 1)
  :bind
  ("C-." . undo)
  ("C-j" . newline)
  ("M-h" . mark-word)
  ("<up>" . beginning-of-buffer)
  ("<down>" . end-of-buffer)
  ("<home>" . find-file)
  ("<end>" . save-buffer)
  ("<next> a f" . auto-fill-mode)
  ("<next> c w" . count-words)
  ("<next> e ;" . eval-expression)
  ("<next> l l" . list-processes)
  ("<next> s a" . mark-whole-buffer)
  :hook
  ((gitignore-mode . mk-auto-fill-mode)
   (haskell-cabal-mode . mk-auto-fill-mode)
   (prog-mode . mk-auto-fill-mode)
   (proof-mode . mk-auto-fill-mode)
   (text-mode . auto-fill-mode)
   (yaml-mode . mk-auto-fill-mode)))

(use-package smart-mode-line
  :config
  (let ((sml/no-confirm-load-theme t))
    (sml/setup)))

(use-package smartparens
  :demand
  :commands
  (sp-backward-kill-sexp
   sp-backward-sexp
   sp-kill-sexp
   sp-forward-sexp
   sp-select-next-thing
   sp-kill-hybrid-sexp
   sp-add-to-previous-sexp)
  :init
  (setq
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil)
  :config
  (smartparens-global-mode 1)
  (advice-add 'sp-add-to-previous-sexp :after (lambda () (just-one-space)))
  (advice-add 'sp-add-to-previous-sexp :after (lambda () (sp-forward-sexp)))
  (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-cabal-mode)
  (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode)
  :bind
  (:map
   smartparens-mode-map
   ("<C-backspace>" . sp-backward-kill-sexp)
   ("M-b" . sp-backward-sexp)
   ("M-d" . sp-kill-sexp)
   ("M-f" . sp-forward-sexp)
   ("M-h" . sp-select-next-thing)
   ("M-k" . sp-kill-hybrid-sexp)
   ("M-t" . sp-add-to-previous-sexp)))

(use-package swiper
  :bind
  ("C-s" . swiper)
  ("M-S" . swiper-all))

(use-package tabify
  :config
  (defun mk-untabify ()
    "Untabify the current buffer."
    (untabify (point-min) (point-max)))
  :bind
  ("<next> u t" . mk-untabify))

(use-package time
  :demand
  :init
  (setq
   display-time-24hr-format t
   display-time-default-load-average nil)
  :config
  (display-time-mode 1))

(use-package typit
  :bind
  ("<next> t y" . typit-advanced-test))

(use-package visual-regexp
  :bind
  ("<next> q e" . vr/query-replace))

(use-package wdired
  :after (dired)
  :init
  (setq
   wdired-allow-to-change-permissions t)
  :bind
  (:map
   wdired-mode-map
   ("<down>" . mk-dired-last-file)
   ("<up>" . mk-dired-first-file)))

(use-package whitespace
  :init
  (setq-default
   whitespace-line-column 80
   whitespace-style '(face trailing tabs empty lines-tail))
  :hook
  ((gitignore-mode . whitespace-mode)
   (haskell-cabal-mode . whitespace-mode)
   (prog-mode . whitespace-mode)
   (proof-mode . whitespace-mode)
   (text-mode . whitespace-mode)
   (yaml-mode . whitespace-mode)))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1))

(use-package window
  :config

  (defun mk-double-buffer ()
    "Show currect buffer in other window and switch to that window."
    (interactive)
    (if (> (length (window-list)) 1)
        (let ((original-buffer (buffer-name)))
          (other-window 1)
          (switch-to-buffer original-buffer))
      (split-window-sensibly)
      (other-window 1)))

  (defun mk-switch-to-messages ()
    "Switch to the *Messages* buffer."
    (interactive)
    (switch-to-buffer "*Messages*"))

  (defun mk-switch-to-scratch ()
    "Switch to the *scratch* buffer."
    (interactive)
    (switch-to-buffer "*scratch*"))

  :bind
  ("C-<prior>" . switch-to-buffer)
  ("<next> d b" . mk-double-buffer)
  ("<next> h h" . split-window-below)
  ("<next> m m" . mk-switch-to-messages)
  ("<next> s s" . mk-switch-to-scratch)
  ("<next> v v" . split-window-right))

(use-package xref
  :init
  (setq
   xref-after-jump-hook (list #'recenter)
   xref-after-return-hook nil))

(use-package yasnippet
  :demand
  :init
  (setq
   yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-global-mode 1)
  :bind
  ("<next> y a" . yas-reload-all))

(use-package zenburn-theme
  :config
  (when window-system
    (load-theme 'zenburn t)))

(use-package ztree
  :after (dired)
  :commands (ztree-dir)
  :init
  (setq
   ztree-dir-filter-list nil
   ztree-draw-unicode-lines t)
  :config
  (defun mk-ztree-dir ()
    "Show tree of the current ‘default-directory’."
    (interactive)
    (ztree-dir default-directory))
  :bind
  (:map
   dired-mode-map
   ("z" . mk-ztree-dir)))

(use-package zygospore
  :after (modalka)
  :commands (zygospore-toggle-delete-other-windows)
  :bind
  (:map
   modalka-mode-map
   ("I" . zygospore-toggle-delete-other-windows)
   :map
   dired-mode-map
   ("I" . zygospore-toggle-delete-other-windows)))

(use-package zzz-to-char
  :bind
  ("M-z" . zzz-up-to-char))

(provide 'mk-packages)

;;; mk-packages.el ends here
