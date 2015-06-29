;;; mk-abbrev.el --- Collection of abbreviations -*- lexical-binding: t; -*-
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

;; I keep all my abbreviations here. I don't have any abbreviations that are
;; specific to some major mode, so I just edit `global-abbrev-table'. As you
;; can see I use abbreviations mainly for inserting of Unicode symbols that
;; otherwise are difficult to type on standard keyboards.

;;; Code:

(setq save-abbrevs nil)

(define-abbrev-table 'global-abbrev-table
  '(("acc" "́")  ; accent
    ("apeq" "≈")  ; approximately equal
    ("bot"  "⊥")  ; bottom
    ("bull" "•")  ; bullet
    ("copy" "©")  ; copyright sign
    ("dagg" "†")  ; dagger
    ("dagr" "‡")  ; crossed dagger
    ("dash" "—")  ; em dash
    ("dda"  "⇓")  ; double downwards arrow
    ("deg"  "°")  ; degree
    ("dla"  "⇐")  ; double leftwards arrow
    ("dqu"  "“”") ; double quotation marks
    ("dra"  "⇒")  ; double rightwards arrow
    ("dua"  "⇑")  ; double upwards arrow
    ("elli" "…")  ; ellipsis
    ("fleu" "❧")  ; fleuron
    ("guil" "«»") ; guillemets
    ("hash" "#")  ; number sign or hash sign
    ("id"   "≡")  ; identical to
    ("ineg" "∫")  ; integral
    ("ineq" "≠")  ; inequality
    ("inf"  "∞")  ; infinity
    ("inte" "‽")  ; interrobang
    ("intr" "·")  ; interpunct
    ("keyb" "⌨")  ; keyboard
    ("loze" "◊")  ; lozenge
    ("mnpl" "∓")  ; minus-plus
    ("mult" "×")  ; multiplication
    ("nabl" "∇")  ; nabla
    ("ndsh" "–")  ; en dash
    ("num"  "№")  ; numero sign
    ("obel" "÷")  ; obelus
    ("plmn" "±")  ; plus-minus
    ("pnd"  "£")  ; pound
    ("prod" "∏")  ; product
    ("qed"  "■")  ; quod erat demonstrandum
    ("root" "√")  ; root
    ("rub"  "₽")  ; Russian ruble
    ("sda"  "↓")  ; simple downwards arrow
    ("sect" "§")  ; section
    ("sgui" "‹›") ; single arrow guillements
    ("sla"  "←")  ; simple leftwards arrow
    ("squ"  "‘’") ; single quotation marks
    ("sra"  "→")  ; simple rightwards arrow
    ("srcp" "℗")  ; sound recording copyright symbol
    ("star" "★")  ; star
    ("sua"  "↑")  ; simple upwards arrow
    ("sum"  "∑")  ; summation
    ;; Greek alphabet
    ("alpha"   "α") ("Alpha"   "Α")
    ("beta"    "β") ("Beta"    "Β")
    ("gamma"   "Y") ("Gamma"   "Γ")
    ("delta"   "δ") ("Delta"   "Δ")
    ("epsilon" "ε") ("Epsilon" "Ε")
    ("zeta"    "ζ") ("Zeta"    "Ζ")
    ("eta"     "η") ("Eta"     "Η")
    ("theta"   "θ") ("Theta"   "Θ")
    ("iota"    "ι") ("Iota"    "Ι")
    ("kappa"   "κ") ("Kappa"   "Κ")
    ("lambda"  "λ") ("Lambda"  "Λ")
    ("mu"      "μ") ("Mu"      "Μ")
    ("nu"      "ν") ("Nu"      "Ν")
    ("xi"      "ξ") ("Xi"      "Ξ")
    ("omicron" "ο") ("Omicron" "Ο")
    ("pi"      "π") ("Pi"      "Π")
    ("rho"     "ρ") ("Rho"     "Ρ")
    ("sigma"   "σ") ("Sigma"   "Σ") ("fsigma" "ς")
    ("tau"     "τ") ("Tau"     "Τ")
    ("upsilon" "υ") ("Upsilon" "Υ")
    ("phi"     "φ") ("Phi"     "Φ")
    ("chi"     "χ") ("Chi"     "Χ")
    ("psi"     "ψ") ("Psi"     "Ψ")
    ("omega"   "ω") ("Omega"   "Ω"))
  "Abbreviations to insert some Unicode characters automatically.")

(defvar mk-abbrev-map (copy-keymap minibuffer-local-map)
  "This keymap is used when `mk-abbrev-insert' reads its argument.")

(define-key mk-abbrev-map (kbd "SPC") #'exit-minibuffer)

(defvar mk-abbrev-last nil
  "Name of last abbrev expanded with `mk-abbrev-insert' function.")

(defun mk-abbrev-insert (&optional abbrev)
  "Read name of abbreviation ABBREV and insert it.
If input is empty (or it's NIL if the function is called
non-interactively), insert last used abbreviation or if there is
no such abbreviation yet, do nothing.  Good when need to insert
abbreviation with activated input method.

This command is smart enough to place point inside abbreviations
that are pairs of quoting characters, otherwise point is placed
after the expansion.

If there is an active region and expansion is a pair of quoting
characters, wrap them around the region."
  (interactive
   (list
    (let ((input (read-from-minibuffer "Abbrev: " nil mk-abbrev-map)))
      (when (> (length input) 0)
        input))))
  (let* ((abbrev    (or abbrev mk-abbrev-last))
         (expansion (abbrev-expansion abbrev))
         (pairp (and (= (length expansion) 2)
                     (eq (get-char-code-property
                          (elt expansion 0)
                          'general-category)
                         'Pi)
                     (eq (get-char-code-property
                          (elt expansion 1)
                          'general-category)
                         'Pf))))
    (when expansion
      (if (and pairp mark-active)
          (let ((beg (region-beginning))
                (end (1+ (region-end))))
            (goto-char beg)
            (insert (elt expansion 0))
            (goto-char end)
            (insert (elt expansion 1)))
        (insert expansion)
        (when pairp
          (backward-char 1)))
      (setf mk-abbrev-last abbrev))))

(provide 'mk-abbrev)

;;; mk-abbrev.el ends here
