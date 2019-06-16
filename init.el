;;;   straight.el bootstrap
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;;   end


;;;   package.el init
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
(package-initialize)
;;;   end


;;;   Install use-package
(straight-use-package 'use-package)
(setq use-package-always-defer t
      use-package-verbose t
      use-package-always-ensure t)
;;;   end


;;;   straight.el configurations
(setq
 straight-check-for-modifications 'live  ; build package when modified in Emacs
 )
;;;   end


;;;   Load environment variables in Emacs
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))
;;;   end


;;;   Load Emacs internal configurations
(when (file-readable-p "~/.emacs.d/emacs-internals.el")
  (use-package f
    :config
    (f-mkdir "~/.emacs.d/.cache" "auto-save-list")
    (f-mkdir tramp-persistency-file-name))
  (use-package general)
  (load-file "~/.emacs.d/emacs-internals.el"))
;;;   end


;;;   Function to get basename of a given path
(defun basename (path)
    "Returns just the file name of the given PATH."
    (file-name-nondirectory (directory-file-name path)))
;;;   end


;;;   Hungry delete is the best part of editing text!
(use-package hungry-delete
  :init
  (global-hungry-delete-mode t))
;;;   end


;;;   Hide minor modes from modeline
(use-package minions
  :bind ([S-down-mouse-3] . minions-minor-modes-menu)
  :hook (after-init . minions-mode))
;;;   end


;;;   Show last keybind and the function in modeline
(use-package keycast
  :bind ("<f9> k" . keycast-mode))
;;;   end


;;;   Magit for top notch git integration
(use-package magit
  :bind
  (("C-x g" . magit-status)
   :map magit-status-mode-map
   ("q" . project-kill-magit-buffers))
  :init
  (defun project-kill-magit-buffers ()
    "Kill current project's magit buffers."
    (interactive)
    (let ((project-magit-buffers-regexp
           (concat
            "^magit\\(?:\\|-[a-z]*\\): \\(?:"
            (regexp-quote (basename default-directory))
            "\\|"
            (regexp-quote (basename default-directory))
            "\\)")))
      (kill-matching-buffers project-magit-buffers-regexp t t))))
(use-package forge
  :straight
  (forge
   :type git
   :host github
   :repo "magit/forge"))
;;;   end


;;;   Magithub for GitHub integration
(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/github"
        magithub-dir "~/.emacs.d/.cache/magithub"))
;;;   end


;;;   Expand region for smart region selection
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-+" . er/contract-region)))
;;;   end


;;;   Project support is very useful
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (use-package ag)
  :config
  (progn
    (projectile-mode 1)
    (setq projectile-completion-system 'default)))
;;;   end


;;;   Switching windows is a bit hard in Emacs
(use-package switch-window
  :bind* (("M-TAB" . switch-window)))
;;;   end


;;;   Nice to lookup new keys to learn new stuff
(use-package which-key
  :init
  (which-key-mode 1))
;;;   end


;;;   Multiple cursor for small and fast edits
(use-package multiple-cursors
  :bind
  (("C-S-c" . mc/edit-lines)
   ("M-S-<up>" . mc/mark-previous-like-this)
   ("M-<up>" . mc/skip-to-previous-like-this)
   ("M-S-<down>" . mc/mark-next-like-this)
   ("M-<down>" . mc/skip-to-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("M-S-<mouse-1>" . mc/add-cursor-on-click)
   ("M-S-<mouse-2>" . mc/add-cursor-on-click)
   ("M-S-<mouse-3>" . mc/add-cursor-on-click)))
;;;   end


;;;   Undo tree for better visualization of undo in Emacs
(use-package undo-tree
  :bind
  (:map undo-tree-map
        ("C-_" . nil))  ; reserved for move-text-up
  :init
  (global-undo-tree-mode t))
;;;   end


;;;   More verbose Emacs documentation lookup
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))
;;;   end


;;;   Move text in a buffer
(use-package move-text
  :bind
  (("C-_" . move-text-up)
   ("C--" . move-text-down)))
;;;   end


;;;   This helps edit results in a *grep* buffer
;;      C-c C-p - Enable editing in *grep* buffer
;;      C-x C-s - Save changes
;;    Note: This doesn't save to the file
(use-package wgrep)
;;;   end


;;;   The doom theming
(use-package doom-themes
  :init
  (load-theme 'doom-spacegrey))
(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-to-project))
;;;   end


;;;   Sidebar
(use-package treemacs
  :straight t
  :bind ("<f9> t" . treemacs))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (use-package vscode-icon
    :straight
    (vscode-icon
     :type git :host github
     :repo "jojojames/vscode-icon-emacs")
    :commands (vscode-icon-for-file))
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))
;;;   end


;;;   Page break char doesn’t look good
(use-package page-break-lines
  :init
  (global-page-break-lines-mode t))
;;;   end


;;;   Completion
(use-package company
  :hook
  (after-init . global-company-mode)
  :bind
  ("<C-j>" . company-complete)
  :config
  (setq company-idle-delay 0.09
        company-minimum-prefix-length 5
        company-selection-wrap-around t
        company-show-numbers t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-backends '(company-lsp company-nxml company-cmake
                                       company-css company-capf
                                       (company-dabbrev-code company-keywords)
                                       company-files company-dabbrev)
        company-jedi-python-bin "python"))
;;;   end


;;;   Language Server Protocol
(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  (require 'lsp-clients)
  (add-hook 'prog-mode-hook 'lsp))
(use-package company-lsp
  :straight t
  :commands company-lsp)
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-doc-use-webkit t)
  :config
  (setq lsp-ui-doc-enable t
        lsp-enable-completion-at-point t
        ;lsp-ui-doc-position 'at-point
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil))
;;;   end


;;;   Python
(use-package anaconda-mode
  :hook
  ((python-mode . anaconda-mode)
   (python-mode . anaconda-eldoc-mode))
  :init
  (progn
    (use-package company-anaconda
      :config
      (require 'rx))
    (with-eval-after-load 'company
      (add-to-list 'company-backends '(company-anaconda :with company-capf)))
    (setq anaconda-mode-installation-directory "~/.emacs.d/.cache/anaconda-mode")))

(use-package pyvenv)
(use-package pipenv
  :bind
  (("<f9> l p v a" . pipenv-activate)
   ("<f9> l p v d" . pipenv-deactivate)
   ("<f9> l p v g" . pipenv-graph)
   ("<f9> l p v e" . pipenv-envs)))

(use-package pony-mode
  :bind
  (("<f9> l p d a f" . pony-fabric)
   ("<f9> l p d a d" . pony-fabric-deploy)
   ("<f9> l p d f s" . pony-goto-settings)
   ("<f9> l p d f c" . pony-setting)
   ("<f9> l p d f t" . pony-goto-template)
   ("<f9> l p d f r" . pony-resolve)
   ("<f9> l p d i d" . pony-db-shell)
   ("<f9> l p d i s" . pony-shell)
   ("<f9> l p d m " . pony-manage)
   ("<f9> l p d r d" . pony-stopserver)
   ("<f9> l p d r o" . pony-browser)
   ("<f9> l p d r r" . pony-restart-server)
   ("<f9> l p d r u" . pony-runserver)
   ("<f9> l p d r t" . pony-temp-server)
   ("<f9> l p d s c" . pony-south-convert)
   ("<f9> l p d s h" . pony-south-schemamigration)
   ("<f9> l p d s i" . pony-south-initial)
   ("<f9> l p d s m" . pony-south-migrate)
   ("<f9> l p d s s" . pony-syncdb)
   ("<f9> l p d t d" . pony-test-down)
   ("<f9> l p d t e" . pony-test-goto-err)
   ("<f9> l p d t o" . pony-test-open)
   ("<f9> l p d t t" . pony-test)
   ("<f9> l p d t u" . pony-test-up)))

;;;   end


;;;   C and C++
(use-package ccls
  :init
  (require 'ccls))
;;;   end

;;;   Complete almost everything in Emacs using ivy
(use-package ivy
  :hook (after-init . ivy-mode)
  :init
  (setq
   ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) "
   ivy-height 15))
(use-package flx)
(use-package swiper)
(use-package counsel
  :bind
  (("M-x" . counsel-M-x)))
;;;   end


;;;   Scala
(use-package ensime)
;;;   end


;;;   Finest mode for multiple HTML based modes
(use-package web-mode
  :mode ("\\.vue\\'" "\\.html\\'" "\\.htm\\'"))
;;;   end


;;;   Emmet is wonderful
(use-package emmet-mode
  :hook (web-mode . emmet-mode))
;;;   end


;;;   Hot reloading
(use-package http
  :bind
  (("<f9> h e" . httpd-start)
   ("<f9> h d" . httpd-stop)))
(use-package impatient-mode
  :bind ("<f9> i" . impatient-mode))
;;;   end


;;;   ox-reveal for presentations
(use-package ox-reveal)
;;;   end


;;;   Highlight indentation in Emacs
(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :init
  (setq indent-guide-char "."
        indent-guide-delay 0.4))
;;;   end


;;;   MVC framework in Emacs
(use-package rem
  :straight
  (rem
   :type git :host github
   :repo "baygeldin/rem.el"))
;;;   end


;;;   Evil mode
(setq evil-want-keybinding nil)
(use-package evil)
(use-package evil-collection)
;;;   end


;;;   Elf mode
(use-package elf-mode)
;;;   end


;;;   Cmake mode
(use-package cmake-mode
  :ensure t)
;;;   end
;;;   Rust
(use-package rustic
  :init
  (setq rustic-rls-pkg 'lsp-mode))
;;;   end


;;;   Org mode
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture))
  :bind
  (:map org-mode-map
        ("M-n" . outline-next-visible-heading)
        ("M-p" . outline-previous-visible-heading))
  :custom
  (org-return-follows-link t)
  (org-agenda-diary-file "~/.org/diary.org")
  (org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  :config
  (add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp :tangle yes?\n\n#+END_SRC")))

(add-hook 'org-mode-hook
          '(lambda ()
             (setq line-spacing 0.2) ;; Add more line padding for readability
             ))

(setq org-startup-indented t
      org-hide-emphasis-markers t
      org-pretty-entities t)

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
    "** TODO %?\n  %i\n  %a")
   ("l" "Link" entry (file+headline "~/notes.org" "Links")
    "** %T %^L \n%?")
   ))

(use-package ox-hugo
  :after ox
  :config
  (dolist (ext '("zip" "ctf"))
    (push ext org-hugo-external-file-extensions-allowed-for-copying)))
;;;   end


;;;   PlantUML
(use-package plantuml-mode
  :init
  (setq plantuml-jar-path "~/Downloads/plantuml.jar"))
;;;   end


;;;   Emacs Application Framework
(use-package eaf
  :straight (eaf
             :type git
             :host github
             :repo "manateelazycat/emacs-application-framework"))
;;;   end


;;;   Snippet completion
(use-package yasnippet
  :hook
  ((prog-mode . yas-minor-mode)))
(use-package yasnippet-snippets)
;;;   end
