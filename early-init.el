;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026  Abhishek(Compro) Prasad

;; Author: Abhishek(Compro) Prasad
;; Keywords: emacs, configuration, elisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Comment


;;; Code:

(setq debug-on-error  t
      init-file-debug t)

(defalias 'ft 'file-truename)
(defvaralias 'emacs-d 'user-emacs-directory)

(when (<= emacs-major-version 29)
  (add-to-list 'load-path (expand-file-name "user-lisp" emacs-d)))

(setq cache-d (locate-user-emacs-file (concat emacs-d ".cache/"))
      package-user-dir (concat cache-d "elpa/"))

;; (use-package server
;;   :config
;;   (when (not (server-running-p))
;;     (let ((server-file (concat cache-d "server/server")))
;;       (when (file-exists-p server-file)
;;         (delete-file server-file)
;;         (message "Old server file deleted")))
;;     (message "Starting server")
;;     (server-start)))

(menu-bar-mode 0)
(menu-bar-no-scroll-bar)
(blink-cursor-mode 0)
(tool-bar-mode 0)

(delete-selection-mode 1)

(when (not window-system)
  (xterm-mouse-mode 1))  ; Enable mouse in terminal

;; start the initial frame maximized
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; TODO: This variable doesn't work when Emacs starts
(setq compro/font-name (cond
                        ((find-font (font-spec :name "Source Code Pro")) "Source Code Pro-16")
                        ((find-font (font-spec :name "Fira Code")) "Fira Code-16")
                        ((find-font (font-spec :name "Ubuntu Mono")) "Ubuntu Mono-16")
                        ((find-font (font-spec :name "Noto Mono")) "Noto Mono-16")
                        ((find-font (font-spec :name "Input Mono")) "Input Mono-16")
                        ((find-font (font-spec :name "DejaVu Sans Mono")) "Dejavu Sans Mono-16")
                        ((find-font (font-spec :name "Monospace")) "Monospace-16")))

;; This modifies both of the above
(modify-all-frames-parameters `((fullscreen . maximized)
                                (font . "Source Code Pro-11")))
