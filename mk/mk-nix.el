;;; mk-nix.el --- Nix helpers -*- lexical-binding: t; -*-
;;
;; Copyright © 2018 Mark Karpov <markkarpov92@gmail.com>
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

;; Some helpers to find options and packages quicker.

;;; Code:

(require 'mk-utils)

(defun mk-nixos-package (symbol)
  "Find information about a NixOS package given SYMBOL."
  (interactive (list (mk-grab-input "NixOS package: ")))
  (browse-url
   (concat "https://nixos.org/nixos/packages.html#"
           (url-hexify-string symbol))))

(defun mk-nixos-option (symbol)
  "Find information abouta NixOS option given SYMBOL."
  (interactive (list (mk-grab-input "NixOS option: ")))
  (browse-url
   (concat "https://nixos.org/nixos/options.html#"
           (url-hexify-string symbol))))

(provide 'mk-nix)

;;; mk-nix.el ends here
