;;; mk-package.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Custom function for package management.

;;; Code:

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

(provide 'mk-package)

;;; mk-package.el ends here
