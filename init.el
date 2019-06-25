;; ;;;   Stop loading init.el at a specific point
;; (with-current-buffer " *load*"
;;   (goto-char (point-max)))
;; ;;;   end


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


;;;   leaf is a better alternative to use-package
(unless (package-installed-p 'leaf)
  (unless (assoc 'leaf package-archive-contents)
    (package-refresh-contents))
  (condition-case err
      (package-install 'leaf)
    (error
     (package-refresh-contents)       ; renew local melpa cache if fail
     (package-install 'leaf))))
(leaf leaf
  :custom ((leaf-defaults . '(:ensure t))))
;;;   end


;;;   Load environment variables in Emacs
(leaf exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))
;;;   end


;;;   Load Emacs internal configurations
(when (file-readable-p "~/.emacs.d/emacs-internals.el")
  (leaf f
    :commands f-mkdir
    :config
    (f-mkdir "~/.emacs.d/.cache" "auto-save-list")
    (f-mkdir tramp-persistency-file-name))
  (leaf general)
  (load-file "~/.emacs.d/emacs-internals.el"))
;;;   end


;;;   Function to get basename of a given path
(defun basename (path)
  "Returns just the file name of the given PATH."
  (file-name-nondirectory (directory-file-name path)))
;;;   end


;;;   Hungry delete is the best part of editing text!
(leaf hungry-delete
  :init
  (global-hungry-delete-mode t))
;;;   end


;;;   Hide minor modes from modeline
(leaf minions
  :bind ([S-down-mouse-3] . minions-minor-modes-menu)
  :hook (after-init . minions-mode))
;;;   end


;;;   Show last keybind and the function in modeline
(leaf keycast
  :bind ("<f9> k" . keycast-mode))
;;;   end


;;;   Git integration
(leaf magit
  :bind (("C-x g" . magit-status))
  :config
  (define-key magit-mode-map [C-tab] nil)
  (define-key magit-mode-map [C-backtab] nil)
  (define-key magit-mode-map [M-tab] nil)
  (with-eval-after-load 'magit-status
    (define-key magit-status-mode-map [C-tab] nil)
    (define-key magit-status-mode-map [C-backtab] nil)
    (define-key magit-status-mode-map [M-tab] nil))
  (with-eval-after-load 'magit-log
    (define-key magit-log-mode-map [C-tab] nil)
    (define-key magit-log-mode-map [C-backtab] nil)
    (define-key magit-log-mode-map [M-tab] nil))
  (with-eval-after-load 'magit-diff
    (define-key magit-diff-mode-map [C-tab] nil)
    (define-key magit-file-section-map [C-tab] nil)
    (define-key magit-hunk-section-map [C-tab] nil)
    (define-key magit-diff-mode-map [C-backtab] nil)
    (define-key magit-file-section-map [C-backtab] nil)
    (define-key magit-hunk-section-map [C-backtab] nil)
    (define-key magit-diff-mode-map [M-tab] nil)
    (define-key magit-file-section-map [M-tab] nil)
    (define-key magit-hunk-section-map [M-tab] nil)))
(leaf git-messenger
  :bind (("C-x v p" . git-messenger:popup-message)))
;;;   end


;;;   Expand region for smart region selection
(leaf expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-+" . er/contract-region)))
;;;   end


;;;   Project support is very useful
(leaf projectile
  :bind (("C-c p" . projectile-command-map))
  :init
  (leaf ag)
  :config
  (setq projectile-completion-system 'ivy)
  :custom ((projectile-mode . t)))
;;;   end


;;;   Switching windows is a bit hard in Emacs
(leaf switch-window
  :bind* (("M-TAB" . switch-window)))
;;;   end


;;;   Nice to lookup new keys to learn new stuff
(leaf which-key
  :init
  (which-key-mode 1))
;;;   end


;;;   Multiple cursor for small and fast edits
(leaf multiple-cursors
  :bind
  (("C-S-c" . mc/edit-lines)
   ("M-S-<up>" . mc/mark-previous-like-this)
   ("M-<up>" . mc/skip-to-previous-like-this)
   ("M-S-<down>" . mc/mark-next-like-this)
   ("M-<down>" . mc/skip-to-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("M-S-<mouse-1>" . mc/add-cursor-on-click)
   ("M-S-<mouse-2>" . mc/add-cursor-on-click)
   ("M-S-<mouse-3>" . mc/add-cursor-on-click))
  :init
  (leaf phi-search-mc
    :hook (isearch-mode . phi-search-from-isearch-mc/setup-keys)
    :config
    (phi-search-mc/setup-keys)))
;;;   end


;;;   Undo tree for better visualization of undo in Emacs
(leaf undo-tree
  :bind
  (:undo-tree-map
   ("C-_" . nil))  ; reserved for move-text-up
  :init
  (global-undo-tree-mode t))
;;;   end


;;;   More verbose Emacs documentation lookup
(leaf helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))
;;;   end


;;;   Move text in a buffer
(leaf move-text
  :bind
  (("C-_" . move-text-up)
   ("C--" . move-text-down)))
;;;   end


;;;   This helps edit results in a *grep* buffer
;;      C-c C-p - Enable editing in *grep* buffer
;;      C-x C-s - Save changes
;;    Note: This doesn't save to the file
(leaf wgrep)
;;;   end


;;;   The doom theming
(leaf doom-themes)
(leaf doom-modeline
  :hook (after-init-hook . doom-modeline-init)
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-to-project))
;;;   end


;;;   Sidebar
(leaf treemacs
  :load-path "~/Downloads/github.com/Alexander-Miller/treemacs/src/elisp"
  :bind ("<f9> t" . treemacs))

(leaf dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (leaf vscode-icon
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
(leaf page-break-lines
  :init
  (global-page-break-lines-mode t))
;;;   end


;;;   Completion
(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(leaf company
  :hook (after-init-hook . global-company-mode)
  :bind
  (("C-c C-SPC" . company-complete)
   (company-active-map
    ("RET" . nil)
    ("C-h" . nil)
    ("ESC" . company-abort)
    ("<tab>" . company-complete-selection)))
  :config
  (setq company-idle-delay 0.09
        company-minimum-prefix-length 1
        company-show-numbers t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-backends '(company-lsp company-nxml company-cmake
                                       company-css company-capf
                                       (company-dabbrev-code company-keywords)
                                       company-files company-dabbrev)
        company-jedi-python-bin "python")
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(leaf company-posframe
  :config (company-posframe-mode 1))
;;;   end


;;;   Language Server Protocol
(leaf lsp-mode
  :commands lsp
  :init
  (require 'lsp-clients)
  (add-hook 'prog-mode-hook 'lsp))
(leaf company-lsp
  :commands company-lsp)
(leaf lsp-ui
  :hook (lsp-mode-hook . lsp-ui-mode)
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


(leaf py-autopep8)
(leaf pyvenv)
(leaf pipenv
  :bind
  (("<f9> p v a" . pipenv-activate)
   ("<f9> p v d" . pipenv-deactivate)
   ("<f9> p v g" . pipenv-graph)
   ("<f9> p v e" . pipenv-envs)))

(leaf pony-mode
  :bind
  (("<f9> p d a f" . pony-fabric)
   ("<f9> p d a d" . pony-fabric-deploy)
   ("<f9> p d f s" . pony-goto-settings)
   ("<f9> p d f c" . pony-setting)
   ("<f9> p d f t" . pony-goto-template)
   ("<f9> p d f r" . pony-resolve)
   ("<f9> p d i d" . pony-db-shell)
   ("<f9> p d i s" . pony-shell)
   ("<f9> p d m " . pony-manage)
   ("<f9> p d r d" . pony-stopserver)
   ("<f9> p d r o" . pony-browser)
   ("<f9> p d r r" . pony-restart-server)
   ("<f9> p d r u" . pony-runserver)
   ("<f9> p d r t" . pony-temp-server)
   ("<f9> p d s c" . pony-south-convert)
   ("<f9> p d s h" . pony-south-schemamigration)
   ("<f9> p d s i" . pony-south-initial)
   ("<f9> p d s m" . pony-south-migrate)
   ("<f9> p d s s" . pony-syncdb)
   ("<f9> p d t d" . pony-test-down)
   ("<f9> p d t e" . pony-test-goto-err)
   ("<f9> p d t o" . pony-test-open)
   ("<f9> p d t t" . pony-test)
   ("<f9> p d t u" . pony-test-up)))
;;;   end


;;;   C and C++
(leaf ccls
  :init
  (require 'ccls))
;;;   end

;;;   Complete almost everything in Emacs using ivy
(leaf ivy
  :hook (after-init-hook . ivy-mode)
  :init
  (setq
   ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) "
   ivy-height 15
   ivy-more-chars-alist '((t . 1))))
(leaf ivy-posframe
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))
(leaf flx)
(leaf swiper)
(leaf counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-c s r" . counsel-rg)
   ("C-c s a" . counsel-ag)
   ("C-c s g" . counsel-grep)
   ("C-c C-r" . counsel-recentf)
   ("C-c y" . counsel-yank-pop)
   ("C-c u" . counsel-unicode)
   ("C-c r" . ivy-resume)))
;;;   end


;;;   Scala
(leaf ensime)
;;;   end


;;;   Finest mode for multiple HTML based modes
(leaf web-mode
  :mode ("\\.vue\\'" "\\.html\\'" "\\.htm\\'"))
;;;   end


;;;   Emmet is wonderful
(leaf emmet-mode
  :hook web-mode-hook)
;;;   end


;;;   Hot reloading
(leaf http
  :bind
  (("<f9> h e" . httpd-start)
   ("<f9> h d" . httpd-stop)))
(leaf impatient-mode
  :bind ("<f9> i" . impatient-mode))
;;;   end


;;;   Highlight indentation in Emacs
(leaf indent-guide
  :hook (prog-mode-hook . indent-guide-mode)
  :init
  (setq indent-guide-char "."
        indent-guide-delay 0.4))
;;;   end


;;;   Evil mode
(setq evil-want-keybinding nil)
(leaf evil)
(leaf evil-collection)
;;;   end


;;;   Elf mode
(leaf elf-mode)
;;;   end


;;;   Cmake mode
(leaf cmake-mode
  :ensure t)
;;;   end


;;;   TODO: Manage system packages
(leaf system-packages)
;;;   end


;;;   Rust
(leaf rustic
  :init
  (setq rustic-rls-pkg 'lsp-mode))
;;;   end


;;;   Org mode
(leaf org
  :leaf-defer t
  :require t
  :bind
  (("C-c l" . org-store-link)
   ("C-c b" . org-switchb)
   (:org-mode-map
    ("M-n" . outline-next-visible-heading)
    ("M-p" . outline-previous-visible-heading)))
  :custom
  ((org-return-follows-link . t)
   (org-agenda-diary-file . "~/.org/diary.org")
   (org-babel-load-languages . '((emacs-lisp . t) (python . t))))
  :config
  (require 'ox-hugo)
  (require 'org-re-reveal)
  (leaf org-capture :bind (("C-c c" . org-capture)))
  (leaf org-agenda  :bind (("C-c a" . org-agenda)))
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

(leaf ox-hugo
  :config
  (dolist (ext '("zip" "ctf"))
    (push ext org-hugo-external-file-extensions-allowed-for-copying)))


(leaf org-re-reveal)
;;;   end


;;;   PlantUML
(leaf plantuml-mode
  :init
  (setq plantuml-jar-path "~/Downloads/plantuml.jar"))
;;;   end


;;;   Snippet completion
(leaf yasnippet
  :hook (prog-mode-hook . yas-minor-mode))
(leaf yasnippet-snippets)
;;;   end


;;;   Syntax checking
(leaf flycheck)
;;;   end

(defmacro p (form)
  "Output pretty `macroexpand-1'ed form of given FORM."
  `(progn
     (pp (macroexpand-1 ',form))
     nil))


;;;   Tabs
(leaf tabbar-ruler
  :leaf-defer nil
  :bind (([C-tab]           . tabbar-forward-tab)
         ([C-S-iso-lefttab] . tabbar-backward-tab)
         ([C-f4]            . kill-current-buffer)
         (:org-mode-map
          :package org
          ([C-tab]           . tabbar-forward-tab)
          ([C-S-tab]         . tabbar-backward-tab)
          ([C-S-iso-lefttab] . tabbar-backward-tab)
          ([C-f4]            . kill-current-buffer))
         (:magit-mode-map
          :package magit
          ([C-tab]           . tabbar-forward-tab)
          ([C-S-tab]         . tabbar-backward-tab)
          ([C-S-iso-lefttab] . tabbar-backward-tab)
          ([C-f4]            . kill-current-buffer)))
  :custom-face ((tabbar-button              . '((t (:inherit default :box nil :height 104 :width normal :family "Sans Serif"))))
                (tabbar-highlight           . '((t nil)))
                (tabbar-selected            . '((t (:inherit default :weight normal :height 160 :width normal :family "Sans Serif"))))
                (tabbar-selected-modified   . '((t (:inherit default :foreground "red" :box nil :weight normal :height 160 :family "Sans Serif"))))
                (tabbar-unselected          . '((t (:inherit tabbar-selected :background "#fee" :foreground "#333" :height 160))))
                (tabbar-unselected-modified . '((t (:inherit tabbar-selected-modified :background "#fee" :height 160)))))
  :config
  (defun compro/tabbar-buffer-groups ()
    "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
    (list
     (let ((project-root (projectile-project-p))
           (project-name (projectile-project-name)))
       (cond
        ((string-equal "*" (substring (buffer-name) 0 1)) "Others")
        ((eq major-mode 'magit-status-mode) "git status")
        ((eq major-mode 'dired-mode) "Dired")
        ((not (null project-root)) project-name)))))

  ;; (setq tabbar-buffer-groups-function 'compro/tabbar-buffer-groups)
  ;; (tabbar-mode 1)
  )
;;;   end


;;;   Typescript support
(leaf typescript-mode)
;;;   end


(setq leaf-defaults nil)
(leaf navbar
  :load-path "~/Downloads/github.com/conao3/navbar.el"
  :require t
  :config
  (setq navbar-item-list '("Hello, World!" "Hello")))
