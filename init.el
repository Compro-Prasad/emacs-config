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
;;;   end


;;;   straight.el configurations
(setq
 straight-use-package-by-default t       ; use-package should use straight.el
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
  (use-package f)
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
    (when (> (count-windows) 1)
      (delete-window))
    (let ((project-magit-buffers-regexp
           (concat
            "^magit\\(?:\\|-[a-z]*\\): \\(?:"
            (regexp-quote (basename default-directory))
            "\\|"
            (regexp-quote (basename default-directory))
            "\\)")))
      (kill-matching-buffers project-magit-buffers-regexp t t))))
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
  :bind
  (("C-\\" . switch-window)))
;;;   end


;;;   Nice to lookup new keys to learn new stuff
(use-package which-key
  :init
  (which-key-mode 1))
;;;   end


;;;   Multiple cursor for small and fast edits
(use-package multiple-cursors
  :straight
  (multiple-cursors
   :type git
   :host github
   :repo "magnars/multiple-cursors.el"
   :branch "wrap-around")
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
(use-package doom-themes)
;;;   end


;;;   Sidebar
(use-package treemacs
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
  :init
  (progn
    (require 'lsp-imenu)
    (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
    (use-package company-lsp)
    (use-package lsp-ui
      :bind
      (:map lsp-ui-mode-map
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references)))))
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
(use-package pipenv)
;;;   end


;;;   C and C++
(use-package ccls
  :commands lsp-ccls-enable
  :init
  (defun ccls/enable ()
    "Enable ccls in current buffer."
    (interactive)
    (condition-case nil
        (lsp-ccls-enable)
      (user-error nil)))

  (defun ccls/enable-hook ()
    "Enable ccls whenever a C/C++ file opens."
    (interactive)
    (add-hook 'c-mode-common-hook #'ccls/enable))

  (defun ccls/disable-hook ()
    "Remove ccls from c-mode-common-hook."
    (interactive)
    (remove-hook 'c-mode-common-hook #'ccls/enable))
  (with-eval-after-load 'c++-mode
    (define-key c++-mode-map (kbd "M-.") 'lsp-ui-peek-jump-forward)
    (define-key c++-mode-map (kbd "M-,") 'lsp-ui-peek-jump-backward)))
;;;   end


;;;   Complete almost everything in Emacs using ivy
(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (progn
    (use-package flx)
    (setq
     ivy-use-virtual-buffers t
     ivy-count-format "(%d/%d) "
     ivy-initial-inputs-alist nil
     ivy-re-builders-alist
     '((projectile-find-file . ivy--regex-fuzzy)
       (projectile-find-dir . ivy--regex-fuzzy)
       (projectile-switch-to-buffer . ivy--regex-fuzzy)
       (projectile-switch-project . ivy--regex-fuzzy))
     ivy-height 15)))
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


;;;   Code cycling
(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)
              ("<backtab>" . bicycle-cycle-global)))

(with-eval-after-load 'prog-mode
  (progn
    (add-hook 'prog-mode-hook 'outline-minor-mode)
    (add-hook 'prog-mode-hook 'hs-minor-mode)))
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
(use-package rustic)
(use-package lsp-rust
  :init
  (with-eval-after-load 'lsp-mode
    (setq lsp-rust-rls-command '("rustup" "run" "nightly-2018-08-19" "rls"))
    (require 'lsp-rust))
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  (add-hook 'rust-mode-hook #'flycheck-mode))
;;;   end


