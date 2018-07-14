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


;;;   Install use-package
(straight-use-package 'use-package)
;;;   end


;;;   straight.el configurations
(setq
 straight-use-package-by-default t       ; use-package should use straight.el
 straight-check-for-modifications 'live  ; build package when modified in Emacs
 )
;;;   end


;;;   Load Emacs internal configurations
(when (file-readable-p "~/.emacs.d/emacs-internals.el")
  (use-package f)
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
  (projectile-mode 1))
;;;   end


;;;   Switching windows is a bit hard in Emacs
(use-package switch-window
  :bind
  (("M-\\" . switch-window)))
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


;;;   The doom theming
(use-package doom-modeline
  :straight
  (doom-modeline
   :type git
   :host github
   :repo "seagle0128/doom-modeline")
  :hook (after-init . doom-modeline-init))

(use-package doom-themes)
;;;   end


;;;   Sidebar
(use-package treemacs)
;;;   end


;;;   Page break char doesn’t look good
(use-package page-break-lines
  :init
  (global-page-break-lines-mode t))
;;;   end


;;;   Show popups in overlays
(use-package pos-tip
  :config
  (progn
    (defun my-eldoc-display-message (format-string &rest args)
      "Display eldoc message near point."
      (when format-string
        (pos-tip-show (apply 'format format-string args) nil nil nil 20)))
    (setq eldoc-message-function #'my-eldoc-display-message)))
;;;   end


;;;   Completion
(use-package company
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-mode-map
        ("C-<tab>" . company-complete))
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
    (use-package company-lsp)))
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
      (add-to-list 'company-backends '(company-anaconda :with company-capf)))))
;;;   end


;;;   C and C++
(use-package ccls
  :commands lsp-ccls-enable
  :hook (c-mode-common . ccls//enable)
  :init
  (defun ccls//enable ()
    (condition-case nil
        (lsp-ccls-enable)
    (user-error nil))))
;;;   end
