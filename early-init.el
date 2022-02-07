;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020  Abhishek(Compro) Prasad

;; Author: Abhishek(Compro) Prasad
;; Keywords: emacs, configuration, elisp, early init

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

(menu-bar-mode 0)
(menu-bar-no-scroll-bar)
(blink-cursor-mode 0)
(tool-bar-mode 0)

(delete-selection-mode 1)

(when (not window-system)
  (xterm-mouse-mode 1))  ; Enable mouse in terminal

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "gray10" :foreground "#bbc0ca" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 98 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(mode-line ((t (:background "blue" :foreground "white" :box nil))))
 '(org-block-begin-line ((t (:inherit org-meta-line :extend t :background "gray13" :foreground "tan4"))))
 '(region ((t (:extend t :background "gray25"))))
 '(shadow ((t (:foreground "SlateBlue1"))))
 '(tab-bar ((t (:inherit variable-pitch :background "gray26" :foreground "white" :height 1.2))))
 '(tab-bar-tab ((t (:inherit tab-bar :background "gray10" :box nil))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "gray20")))))
