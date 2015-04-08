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
    ("8bull" "•")  ; bullet
    ("8copy" "©")  ; copyright sign
    ("8dagg" "†")  ; dagger
    ("8dagr" "‡")  ; crossed dagger
    ("8dash" "—")  ; em dash
    ("8dda"  "⇓")  ; double downwards arrow
    ("8degr" "°")  ; degree
    ("8delt" "Δ")  ; delta
    ("8dla"  "⇐")  ; double leftwards arrow
    ("8dra"  "⇒")  ; double rightwards arrow
    ("8dua"  "⇑")  ; double upwards arrow
    ("8elip" "…")  ; elipsis
    ("8guil" "«»") ; guillemets
    ("8ineg" "∫")  ; integral
    ("8ineq" "≠")  ; inequality
    ("8inf"  "∞")  ; infinity
    ("8intr" "·")  ; interpunct
    ("8mnpl" "∓")  ; minus-plus
    ("8mult" "×")  ; multiplication
    ("8nabl" "∇")  ; nabla
    ("8num"  "№")  ; numero sign
    ("8obel" "÷")  ; obelus
    ("8pi"   "π")  ; pi
    ("8plmn" "±")  ; plus-minus
    ("8pnd"  "£")  ; pound
    ("8prod" "∏")  ; product
    ("8qed"  "■")  ; quod erat demonstrandum
    ("8root" "√")  ; root
    ("8rub"  "₽")  ; Russian ruble
    ("8sda"  "↓")  ; simple downwards arrow
    ("8sect" "§")  ; section
    ("8sla"  "←")  ; simple leftwards arrow
    ("8sra"  "→")  ; simple rightwards arrow
    ("8sua"  "↑")  ; simple upwards arrow
    ("8sum"  "∑")) ; summation
  "Abbreviations to insert some Unicode characters automatically.")

(provide 'mk-abbrev)

;;; mk-abbrev.el ends here
