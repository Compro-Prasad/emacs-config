;;; init.el --- Things that happen when emacs starts
;;; Commentary:

;;; Code:

;; Load package.el
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Already declared somewhere?
(when (< emacs-major-version 27)
  (package-initialize))


;; Install quelpa-use-package if not installed. Installing
;; quelpa-use-package will install both quelpa and use-package. It
;; will be used to automatically download and lazy load packages.
;; quelpa on the other hand provides a way to install git repositories
;; directly without relying on tarballs.
(when (not (package-installed-p 'quelpa-use-package))
  (package-refresh-contents)
  (package-install 'quelpa-use-package))


;; My configurations inside this beautiful org file
(org-babel-load-file (expand-file-name "all-my-config.org" user-emacs-directory))


(provide 'init)
;;; init.el ends here
