;;; mk-abbrev.el --- Collection of abbreviations -*- lexical-binding: t; -*-
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

;;; I keep all my abbreviations here. I don't have any abbreviations that
;;; are specific to some major mode, so I just edit `global-abbrev-table'.
;;; As you can see I use abbreviations mainly for inserting of Unicode
;;; symbols that otherwise are difficult to type on standard keyboards.

;;; Code:

(setq save-abbrevs nil)

(define-abbrev-table 'global-abbrev-table
  '(("8apeq" "≈")  ; approximately equal
    ("8bot"  "⊥")  ; bottom
    ("8bull" "•")  ; bullet
    ("8copy" "©")  ; copyright sign
    ("8dagg" "†")  ; dagger
    ("8dagr" "‡")  ; crossed dagger
    ("8dash" "—")  ; em dash
    ("8dda"  "⇓")  ; double downwards arrow
    ("8degr" "°")  ; degree
    ("8dla"  "⇐")  ; double leftwards arrow
    ("8dqu"  "“”") ; double quotation marks
    ("8dra"  "⇒")  ; double rightwards arrow
    ("8dua"  "⇑")  ; double upwards arrow
    ("8elli" "…")  ; ellipsis
    ("8guil" "«»") ; guillemets
    ("8id"   "≡")  ; identical to
    ("8ineg" "∫")  ; integral
    ("8ineq" "≠")  ; inequality
    ("8inf"  "∞")  ; infinity
    ("8intr" "·")  ; interpunct
    ("8mnpl" "∓")  ; minus-plus
    ("8mult" "×")  ; multiplication
    ("8nabl" "∇")  ; nabla
    ("8num"  "№")  ; numero sign
    ("8obel" "÷")  ; obelus
    ("8plmn" "±")  ; plus-minus
    ("8pnd"  "£")  ; pound
    ("8prod" "∏")  ; product
    ("8qed"  "■")  ; quod erat demonstrandum
    ("8root" "√")  ; root
    ("8rub"  "₽")  ; Russian ruble
    ("8sda"  "↓")  ; simple downwards arrow
    ("8sect" "§")  ; section
    ("8sla"  "←")  ; simple leftwards arrow
    ("8squ"  "‘’") ; single quotation marks
    ("8sra"  "→")  ; simple rightwards arrow
    ("8sua"  "↑")  ; simple upwards arrow
    ("8sum"  "∑")  ; summation
    ("8alpha"   "α") ("8Alpha"   "Α")
    ("8beta"    "β") ("8Beta"    "Β")
    ("8gamma"   "Y") ("8Gamma"   "Γ")
    ("8delta"   "δ") ("8Delta"   "Δ")
    ("8epsilon" "ε") ("8Epsilon" "Ε")
    ("8zeta"    "ζ") ("8Zeta"    "Ζ")
    ("8eta"     "η") ("8Eta"     "Η")
    ("8theta"   "θ") ("8Theta"   "Θ")
    ("8iota"    "ι") ("8Iota"    "Ι")
    ("8kappa"   "κ") ("8Kappa"   "Κ")
    ("8lambda"  "λ") ("8Lambda"  "Λ")
    ("8mu"      "μ") ("8Mu"      "Μ")
    ("8nu"      "ν") ("8Nu"      "Ν")
    ("8xi"      "ξ") ("8Xi"      "Ξ")
    ("8omicron" "ο") ("8Omicron" "Ο")
    ("8pi"      "π") ("8Pi"      "Π")
    ("8rho"     "ρ") ("8Rho"     "Ρ")
    ("8sigma"   "σ") ("8Sigma"   "Σ") ("8fsigma" "ς")
    ("8tau"     "τ") ("8Tau"     "Τ")
    ("8upsilon" "υ") ("8Upsilon" "Υ")
    ("8phi"     "φ") ("8Phi"     "Φ")
    ("8chi"     "χ") ("8Chi"     "Χ")
    ("8psi"     "ψ") ("8Psi"     "Ψ")
    ("8omega"   "ω") ("8Omega"   "Ω"))
  "Abbreviations to insert some Unicode characters automatically.")

(provide 'mk-abbrev)

;;; mk-abbrev.el ends here
