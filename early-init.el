;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022  Abhishek(Compro) Prasad

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
