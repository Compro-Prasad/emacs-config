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
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
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
(leaf leaf)
;;;   end


;;;   Load Emacs internal configurations
(when (file-readable-p (concat user-emacs-directory "emacs-internals.el"))
  ;; Install general for customizing keybinds
  (leaf general :ensure t)
  (leaf f :ensure t
    :commands f-mkdir
    :config
    (f-mkdir (concat user-emacs-directory ".cache") "auto-save-list")
    (f-mkdir tramp-persistency-file-name))
  (leaf general)
  (load-file (concat user-emacs-directory "emacs-internals.el")))
;;;   end


;;;   Function to get basename of a given path
(defun basename (path)
  "Returns just the file name of the given PATH."
  (file-name-nondirectory (directory-file-name path)))
;;;   end


;;;   Hungry delete is the best part of editing text!
(leaf hungry-delete :ensure t
  :init
  (global-hungry-delete-mode t))
;;;   end


;;;   Hide minor modes from modeline
(leaf minions :ensure t
  :bind ([S-down-mouse-3] . minions-minor-modes-menu)
  :hook (after-init-hook . minions-mode))
;;;   end


;;;   Show last keybind and the function in modeline
(leaf keycast :ensure t
  :bind ("<f9> k" . keycast-mode))
;;;   end


;;;   Git integration
(leaf magit :ensure t
  :bind (("C-x g" . magit-status)
         (magit-mode-map
          ([C-tab] . nil)
          ([C-backtab] . nil)
          ([M-tab] . nil))
         (magit-status-mode-map
          ("q" . compro/kill-magit-buffers)
          ([C-tab] . nil)
          ([C-backtab] . nil)
          ([M-tab] . nil))
         (magit-log-mode-map
          ([C-tab] . nil)
          ([C-backtab] . nil)
          ([M-tab] . nil)))
  :preface
  (leaf forge :after magit :ensure t :require t :leaf-defer nil)
  :config
  (defun compro/kill-magit-buffers ()
    "Kill magit buffers related to a project."
    (interactive)
    (magit-mode-bury-buffer 16))
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
(leaf git-messenger :ensure t
  :bind (("C-x v p" . git-messenger:popup-message)))
;;;   end


;;;   Expand region for smart region selection
(leaf expand-region :ensure t :require t :leaf-defer nil
  :commands (er/expand-region
             er/mark-paragraph
             er/mark-inside-pairs
             er/mark-outside-pairs
             er/mark-inside-quotes
             er/mark-outside-quotes
             er/contract-region)
  :bind (("C-=" . hydra-er/body))
  :config
  (defhydra hydra-er (:hint nil)
    "
^Expand^  ^Reduce^
^──────^──^────^─────────────────
_C-=_     _C-+_
_=_       _+_
        _-_"
    ("C-=" er/expand-region)
    ("=" er/expand-region)
    ("C-+" er/contract-region)
    ("C--" er/contract-region)
    ("+" er/contract-region)
    ("-" er/contract-region)))
;;;   end


;;;   Project support is very useful
(leaf projectile :ensure t
  :bind (("C-c p" . projectile-command-map))
  :init
  (leaf ag :ensure t)
  :config
  (setq projectile-completion-system 'ivy)
  :custom ((projectile-mode . t)))
;;;   end


;;;   Switching windows is a bit hard in Emacs
(leaf switch-window :ensure t
  :bind* (("M-TAB" . switch-window)))
;;;   end


;;;   Nice to lookup new keys to learn new stuff
(leaf which-key :ensure t
  :init
  (which-key-mode 1))
;;;   end


;;;   Multiple cursor for small and fast edits
(leaf multiple-cursors :ensure t
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
  (leaf phi-search-mc :ensure t
    :hook (isearch-mode . phi-search-from-isearch-mc/setup-keys)
    :config
    (phi-search-mc/setup-keys)))
;;;   end


;;;   Undo tree for better visualization of undo in Emacs
(leaf undo-tree :ensure t
  :bind
  (:undo-tree-map
   ("C-_" . nil))  ; reserved for move-text-up
  :init
  (global-undo-tree-mode t))
;;;   end


;;;   More verbose Emacs documentation lookup
(leaf helpful :ensure t :disabled t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))
;;;   end


;;;   Move text in a buffer
(leaf move-text :ensure t
  :bind
  (("C-_" . move-text-up)
   ("C--" . move-text-down)))
;;;   end


;;;   This helps edit results in a *grep* buffer
;;      C-c C-p - Enable editing in *grep* buffer
;;      C-x C-s - Save changes
;;    Note: This doesn't save to the file
(leaf wgrep :ensure t)
;;;   end


;;;   The doom theming
(leaf doom-themes
  :commands (doom-themes-org-config)
  :config
  (doom-themes-org-config)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (when (>= emacs-major-version 27)
    (with-eval-after-load 'org
      (dolist (face '(org-block
                      org-block-begin-line
                      org-block-end-line
                      org-level-1
                      org-quote))
        (set-face-attribute face nil :extend t)))
    (with-eval-after-load 'ediff
      (dolist (face '(ediff-current-diff-A
                      ediff-current-diff-Ancestor
                      ediff-current-diff-B
                      ediff-current-diff-C
                      ediff-even-diff-A
                      ediff-even-diff-Ancestor
                      ediff-even-diff-B
                      ediff-even-diff-C
                      ediff-fine-diff-A
                      ediff-fine-diff-Ancestor
                      ediff-fine-diff-B
                      ediff-fine-diff-C
                      ediff-odd-diff-A
                      ediff-odd-diff-Ancestor
                      ediff-odd-diff-B
                      ediff-odd-diff-C))
        (set-face-attribute face nil :extend t)))
    (with-eval-after-load 'hl-line
      (set-face-attribute 'hl-line nil :extend t))
    (with-eval-after-load 'faces
      (dolist (face '(region
                      secondary-selection))
        (set-face-attribute face nil :extend t)))
    (with-eval-after-load 'markdown-mode
      (dolist (face '(markdown-code-face
                      markdown-pre-face))
        (set-face-attribute face nil :extend t)))))
(leaf kaolin-themes :ensure t)
(leaf chocolate-theme :ensure t)
(leaf doom-modeline :ensure t
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-to-project))
;;;   end


;;;   Emacs 27 tabs tab-line-mode
(unless (version< emacs-version "27")
  (leaf tab-line :leaf-defer nil :require t :disabled t
    :init
    (global-tab-line-mode)
    :config
    (defun tab-line-close-tab (&optional e)
      (interactive "e")
      (let* ((posnp (event-start e))
             (window (posn-window posnp))
             (buffer (get-pos-property 1 'tab (car (posn-string posnp)))))
        (with-selected-window window
          (cond ((cdr (get-buffer-window-list buffer))
                 (cond ((cdr (tab-line-tabs))
                        (if (eq buffer (current-buffer))
                            (bury-buffer)
                          (set-window-prev-buffers nil (assq-delete-all buffer (window-prev-buffers)))
                          (set-window-next-buffers nil (delq buffer (window-next-buffers)))))
                       (t
                        (delete-window window))))
                (t
                 (kill-buffer buffer)
                 (delete-window window))))
        (force-mode-line-update)))
    (setq tab-line-new-tab-choice nil
          tab-line-close-button-show nil)
    (when (fboundp 'doom-color)
      (let ((bg (doom-color 'bg))
            (fg (doom-color 'fg))
            (base1 (doom-color 'base1))
            (box-width 7))
        (set-face-attribute 'tab-line nil :background base1 :foreground fg)
        (set-face-attribute 'tab-line-tab nil :background bg :box (list :line-width box-width :color bg) :weight 'bold)
        (set-face-attribute 'tab-line-tab-inactive nil :background base1 :box (list :line-width box-width :color base1))))))
;;;   end


;;;   Telegram in Emacs
(leaf telega
  :load-path `,(concat user-emacs-directory ".repos/telega.el")
  :bind ("C-c t" . telega)
  :preface
  (leaf visual-fill-column :ensure t)
  :config
  (setq telega-chat-use-markdown-formatting t))
;;;   end


;;;   "All the icons" font
(leaf all-the-icons :ensure t :require t :leaf-defer nil)
;;;   end


;;;   Sidebar
(when window-system
  (use-package treemacs :ensure t
    :commands (treemacs-follow-mode)
    :hook ((after-init . aorst/treemacs-init-setup)
           (treemacs-mode . aorst/treemacs-setup)
           (treemacs-switch-workspace . aorst/treemacs-expand-all-projects)
           (treemacs-mode . aorst/treemacs-setup-title))
    :config
    (leaf treemacs-magit :ensure t)
    (global-set-key (kbd "C-t") 'treemacs-select-window)
    (define-key treemacs-mode-map [mouse-1] 'treemacs-single-click-expand-action)
    (set-face-attribute 'treemacs-root-face nil
                        :foreground (face-attribute 'default :foreground)
                        :height 1.0
                        :weight 'normal)
    (treemacs-create-theme "Atom"
      :config
      (progn
        (treemacs-create-icon
         :icon (format " %s\t"
                       (all-the-icons-octicon
                        "repo"
                        :v-adjust -0.1
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions (root))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon
                        "chevron-down"
                        :height 0.75
                        :v-adjust 0.1
                        :face '(:inherit font-lock-doc-face :slant normal))
                       (all-the-icons-octicon
                        "file-directory"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions (dir-open))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon
                        "chevron-right"
                        :height 0.75
                        :v-adjust 0.1
                        :face '(:inherit font-lock-doc-face :slant normal))
                       (all-the-icons-octicon
                        "file-directory"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions (dir-closed))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon
                        "chevron-down"
                        :height 0.75
                        :v-adjust 0.1
                        :face '(:inherit font-lock-doc-face :slant normal))
                       (all-the-icons-octicon
                        "package"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions (tag-open))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon
                        "chevron-right"
                        :height 0.75
                        :v-adjust 0.1
                        :face '(:inherit font-lock-doc-face :slant normal))
                       (all-the-icons-octicon
                        "package"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions (tag-closed))
        (treemacs-create-icon
         :icon (format "%s\t"
                       (all-the-icons-octicon
                        "tag"
                        :height 0.9
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions (tag-leaf))
        (treemacs-create-icon
         :icon (format "%s\t"
                       (all-the-icons-octicon
                        "flame"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions (error))
        (treemacs-create-icon
         :icon (format "%s\t"
                       (all-the-icons-octicon
                        "stop"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions (warning))
        (treemacs-create-icon
         :icon (format "%s\t"
                       (all-the-icons-octicon
                        "info"
                        :height 0.75
                        :v-adjust 0.1
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions (info))
        (treemacs-create-icon
         :icon (format "  %s\t"
                       (all-the-icons-octicon
                        "file-media"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions ("png" "jpg" "jpeg" "gif" "ico" "tif" "tiff" "svg" "bmp"
                      "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "mkv"
                      "wav" "mp3" "ogg" "midi"))
        (treemacs-create-icon
         :icon (format "  %s\t"
                       (all-the-icons-octicon
                        "file-code"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions ("yml" "yaml" "sh" "zsh" "fish" "c" "h" "cpp" "cxx" "hpp"
                      "tpp" "cc" "hh" "hs" "lhs" "cabal" "py" "pyc" "rs" "el"
                      "elc" "clj" "cljs" "cljc" "ts" "tsx" "vue" "css" "html"
                      "htm" "dart" "java" "kt" "scala" "sbt" "go" "js" "jsx"
                      "hy" "json" "jl" "ex" "exs" "eex" "ml" "mli" "pp" "dockerfile"
                      "vagrantfile" "j2" "jinja2" "tex" "racket" "rkt" "rktl" "rktd"
                      "scrbl" "scribble" "plt" "makefile" "elm" "xml" "xsl" "rb"
                      "scss" "lua" "lisp" "scm" "sql" "toml" "nim" "pl" "pm" "perl"
                      "vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc"
                      "cask" "r" "re" "rei" "bashrc" "zshrc" "inputrc" "editorconfig"
                      "gitconfig"))
        (treemacs-create-icon
         :icon (format "  %s\t"
                       (all-the-icons-octicon
                        "book"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions ("lrf" "lrx" "cbr" "cbz" "cb7" "cbt" "cba" "chm" "djvu"
                      "doc" "docx" "pdb" "pdb" "fb2" "xeb" "ceb" "inf" "azw"
                      "azw3" "kf8" "kfx" "lit" "prc" "mobi" "pkg" "opf" "txt"
                      "pdb" "ps" "rtf" "pdg" "xml" "tr2" "tr3" "oxps" "xps"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon
                                 "file-text"
                                 :v-adjust 0
                                 :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions ("md" "markdown" "rst" "log" "org" "txt"
                      "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon
                                 "file-binary"
                                 :v-adjust 0
                                 :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions ("exe" "dll" "obj" "so" "o" "out"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon
                                 "file-pdf"
                                 :v-adjust 0
                                 :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions ("pdf"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon
                                 "file-zip"
                                 :v-adjust 0
                                 :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon
                                 "file-text"
                                 :v-adjust 0
                                 :face '(:inherit font-lock-doc-face :slant normal)))
         :extensions (fallback))))
    (defun aorst/treemacs-expand-all-projects (&optional _)
      "Expand all projects."
      (interactive)
      (save-excursion
        (treemacs--forget-last-highlight)
        (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
          (-when-let (pos (treemacs-project->position project))
            (when (eq 'root-node-closed (treemacs-button-get pos :state))
              (goto-char pos)
              (treemacs--expand-root-node pos)))))
      (treemacs--maybe-recenter 'on-distance))
    (defun aorst/treemacs-variable-pitch-labels (&rest _)
      (dolist (face '(treemacs-root-face
                      treemacs-git-unmodified-face
                      treemacs-git-modified-face
                      treemacs-git-renamed-face
                      treemacs-git-ignored-face
                      treemacs-git-untracked-face
                      treemacs-git-added-face
                      treemacs-git-conflict-face
                      treemacs-directory-face
                      treemacs-directory-collapsed-face
                      treemacs-file-face
                      treemacs-tags-face))
        (let ((faces (face-attribute face :inherit nil)))
          (set-face-attribute
           face nil :inherit
           `(variable-pitch ,@(delq 'unspecified (if (listp faces) faces (list faces))))))))
    (defun aorst/treemacs-init-setup ()
      "Set treemacs theme, open treemacs, and expand all projects."
      (treemacs-load-theme "Atom")
      (setq treemacs-collapse-dirs 0)
      )
    (defun aorst/treemacs-setup ()
      "Set treemacs buffer common settings."
      (setq tab-width 1
            mode-line-format nil
            line-spacing 5)
      (setq-local scroll-step 1)
      (setq-local scroll-conservatively 10000)
      (set-window-fringes nil 0 0 t)
      (aorst/treemacs-variable-pitch-labels))
    (defun aorst/treemacs-setup-fringes ()
      "Set treemacs buffer fringes."
      (set-window-fringes nil 0 0 t)
      (aorst/treemacs-variable-pitch-labels))
    (advice-add #'treemacs-select-window :after #'aorst/treemacs-setup-fringes)
    (defun aorst/treemacs-ignore (file _)
      (or (s-ends-with? ".elc" file)
          (s-ends-with? ".o" file)
          (s-ends-with? ".a" file)
          (string= file ".svn")))
    (add-to-list 'treemacs-ignored-file-predicates #'aorst/treemacs-ignore)
    (defun aorst/treemacs-setup-title ()
      (let ((format '((:eval (concat
                              (make-string
                               (let ((width (window-width)))
                                 (- (/ (if (= (% width 2) 0) width (1+ width)) 2) 5))
                               ?\ )
                              "Treemacs")))))
        (if (version<= emacs-version "27")
            (setq header-line-format format)
          (setq tab-line-format format)))
      (let ((bg (face-attribute 'default :background))
            (fg (face-attribute 'default :foreground))
            (face (if (version<= emacs-version "27")
                      'header-line
                    'tab-line)))
        (face-remap-add-relative face
                                 :box (list :line-width 7 :color bg)
                                 :background bg :foreground fg :height 1.0))))
  (setq treemacs-width 27
        treemacs-is-never-other-window t
        treemacs-space-between-root-nodes nil
        treemacs-indentation 2)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode nil))
(leaf lsp-treemacs :ensure t)

(leaf dired-sidebar :ensure t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (leaf vscode-icon :ensure t
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
(leaf page-break-lines :ensure t
  :init
  (global-page-break-lines-mode t))
;;;   end


;;;   Completion
(leaf company :ensure t
  :hook (after-init-hook . global-company-mode)
  :bind
  (("S-SPC" . company-complete)
   (company-active-map
    ("C-h" . nil)
    ("ESC ESC" . company-abort)
    ("<tab>" . company-complete-common-or-cycle)))
  :preface
  (leaf company-quickhelp :after company :require t :ensure t
    :config
    (setq company-quickhelp-delay 0.311)
    (company-quickhelp-mode 1))
  (defun compro/company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  :config
  (setq company-idle-delay nil
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
  (setq company-backends (mapcar #'compro/company-mode/backend-with-yas company-backends)))
;;;   end


;;;   Language Server Protocol
(leaf lsp-mode :ensure t
  :init
  (require 'lsp-clients)
  (defun compro/init-lsp ()
    "Start lsp server only when it is a valid project where lsp
is useful."
    (when (and (fboundp 'projectile-project-p) (projectile-project-p))
      (lsp)))
  )
(leaf company-lsp :ensure t
  :commands company-lsp)
(leaf lsp-ui :ensure t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-enable-completion-at-point t
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil))
;; M$ Python Language Server
(leaf lsp-python-ms :ensure t :after lsp-mode :require t
  :preface
  (add-hook 'python-mode-hook
            (lambda ()
              (pipenv-activate)
              (require 'lsp-python-ms)
              (lsp)))
  :init
  (setq lsp-python-ms-executable "~/Downloads/github.com/Microsoft/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))
;; C and C++
(leaf ccls :ensure t :after lsp-mode :require t)
;;;   end


(leaf py-autopep8 :ensure t)
(leaf pyvenv :ensure t)
(leaf pipenv :ensure t
  :bind
  (("<f9> p v a" . pipenv-activate)
   ("<f9> p v d" . pipenv-deactivate)
   ("<f9> p v g" . pipenv-graph)
   ("<f9> p v e" . pipenv-envs)))

(leaf pony-mode :ensure t
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


;;;   Complete almost everything in Emacs using ivy
(leaf ivy :ensure t
  :hook (after-init-hook . ivy-mode)
  :init
  (setq
   ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) "
   ivy-height 15
   ivy-more-chars-alist '((t . 1))))
(leaf ivy-rich :ensure t :require t :after ivy)
(leaf ivy-posframe :ensure t :require t :after ivy
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; TODO: Disable when using EXWM
  ;; UPDATE: It currently works on EXWM
  (ivy-posframe-mode 1))
(leaf prescient :ensure t :require t
  :config
  (prescient-persist-mode 1))
(leaf ivy-prescient :ensure t
  :after ivy
  :config
  (ivy-prescient-mode t)
  (push 'counsel-recentf ivy-prescient-sort-commands))
(leaf company-prescient :ensure t
  :after company
  :config
  (company-prescient-mode t))
(leaf swiper :ensure t)
(leaf counsel :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-c s r" . counsel-rg)
   ("C-c s a" . counsel-ag)
   ("C-c s g" . counsel-grep)
   ("C-c r" . counsel-recentf)
   ("C-c y" . counsel-yank-pop)
   ("C-c u" . counsel-unicode-char)
   ("C-c R" . ivy-resume)))
;;;   end


;;;   Scala - TODO: Look for metals
;;; (leaf ensime :ensure t)
;;;   end


;;;   Finest mode for multiple HTML based modes
(leaf web-mode :ensure t
  :mode ("\\.vue\\'" "\\.html\\'" "\\.htm\\'"))
;;;   end


;;;   Emmet is wonderful
(leaf emmet-mode :ensure t
  :hook web-mode-hook)
;;;   end


;;;   Hot reloading
(leaf impatient-mode :ensure t
  :bind (("<C-i>" . hydra-imp/body))
  :preface
  (leaf http :ensure t)
  :config
  (defhydra hydra-imp (:hint nil)
    "
^Impatient Mode    ^Httpd
^──────────────────^──────────────────^
_<C-i>_, _i_: Toggle  _s_: Start
                  _S_: Stop"
    ("<C-i>" impatient-mode :color pink)
    ("i" impatient-mode :color pink)
    ("s" httpd-start :color pink)
    ("S" httpd-stop :color pink)))
;;;   end


;;;   Highlight indentation in Emacs
(leaf indent-guide :ensure t :disabled t
  :hook (prog-mode-hook . indent-guide-mode)
  :init
  (setq indent-guide-char "."
        indent-guide-delay 0.4))
;;;   end


;;;   Evil mode
(setq evil-want-keybinding nil)
(leaf evil :ensure t)
(leaf evil-collection :ensure t)
;;;   end


;;;   Elf mode
(leaf elf-mode :ensure t)
;;;   end


;;;   Cmake mode
(leaf cmake-mode :ensure t)
;;;   end


;;;   TODO: Manage system packages
(leaf system-packages :ensure t)
;;;   end


;;;   Rust
(leaf rustic :ensure t
  :init
  (setq rustic-rls-pkg 'lsp-mode))
;;;   end


;;;   Org mode
(leaf org :ensure t
  :preface
  (leaf org-plus-contrib :ensure t)
  (leaf ox-hugo :require t :ensure t :after ox :disabled t
    :config
    (dolist (ext '("zip" "ctf"))
      (push ext org-hugo-external-file-extensions-allowed-for-copying)))
  (leaf org-re-reveal :ensure t :require t :after ox)
  (add-hook 'org-mode-hook
            '(lambda ()
               (setq line-spacing 0.2) ;; Add more line padding for readability
               ))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   (:org-mode-map
    :package org
    ([C-tab] . nil)
    ([C-backtab] . nil)
    ("M-n" . outline-next-visible-heading)
    ("M-p" . outline-previous-visible-heading)))
  :custom
  ((org-return-follows-link . t)
   (org-agenda-diary-file . "~/.org/diary.org")
   (org-babel-load-languages . '((emacs-lisp . t) (python . t))))
  :config
  (require 'org-tempo)
  (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
    "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
    :global t
    (if unpackaged/org-export-html-with-useful-ids-mode
        (progn
          (advice-add #'org-export-new-title-reference :override #'unpackaged/org-export-new-title-reference)
          (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference))
      (advice-remove #'org-export-new-title-reference #'unpackaged/org-export-new-title-reference)
      (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))

  (defun unpackaged/org-export-get-reference (datum info)
    "Like `org-export-get-reference', except uses heading titles instead of random numbers."
    (let ((cache (plist-get info :internal-references)))
      (or (car (rassq datum cache))
          (let* ((crossrefs (plist-get info :crossrefs))
                 (cells (org-export-search-cells datum))
                 ;; Preserve any pre-existing association between
                 ;; a search cell and a reference, i.e., when some
                 ;; previously published document referenced a location
                 ;; within current file (see
                 ;; `org-publish-resolve-external-link').
                 ;;
                 ;; However, there is no guarantee that search cells are
                 ;; unique, e.g., there might be duplicate custom ID or
                 ;; two headings with the same title in the file.
                 ;;
                 ;; As a consequence, before re-using any reference to
                 ;; an element or object, we check that it doesn't refer
                 ;; to a previous element or object.
                 (new (or (cl-some
                           (lambda (cell)
                             (let ((stored (cdr (assoc cell crossrefs))))
                               (when stored
                                 (let ((old (org-export-format-reference stored)))
                                   (and (not (assoc old cache)) stored)))))
                           cells)
                          (when (org-element-property :raw-value datum)
                            ;; Heading with a title
                            (unpackaged/org-export-new-title-reference datum cache))
                          ;; NOTE: This probably breaks some Org Export
                          ;; feature, but if it does what I need, fine.
                          (org-export-format-reference
                           (org-export-new-reference cache))))
                 (reference-string new))
            ;; Cache contains both data already associated to
            ;; a reference and in-use internal references, so as to make
            ;; unique references.
            (dolist (cell cells) (push (cons cell new) cache))
            ;; Retain a direct association between reference string and
            ;; DATUM since (1) not every object or element can be given
            ;; a search cell (2) it permits quick lookup.
            (push (cons reference-string datum) cache)
            (plist-put info :internal-references cache)
            reference-string))))

  (defun unpackaged/org-export-new-title-reference (datum cache)
    "Return new reference for DATUM that is unique in CACHE."
    (cl-macrolet ((inc-suffixf (place)
                               `(progn
                                  (string-match (rx bos
                                                    (minimal-match (group (1+ anything)))
                                                    (optional "--" (group (1+ digit)))
                                                    eos)
                                                ,place)
                                  ;; HACK: `s1' instead of a gensym.
                                  (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                             (match-string 2 ,place)))
                                          (suffix (if suffix
                                                      (string-to-number suffix)
                                                    0)))
                                    (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
      (let* ((title (org-element-property :raw-value datum))
             (ref (url-hexify-string (substring-no-properties title)))
             (parent (org-element-property :parent datum)))
        (while (--any (equal ref (car it))
                      cache)
          ;; Title not unique: make it so.
          (if parent
              ;; Append ancestor title.
              (setf title (concat (org-element-property :raw-value parent)
                                  "--" title)
                    ref (url-hexify-string (substring-no-properties title))
                    parent (org-element-property :parent parent))
            ;; No more ancestors: add and increment a number.
            (inc-suffixf ref)))
        ref)))
  (defun org-generate-custom-ids-based-on-headings ()
    (interactive)
    (let ((hlist nil))
      (save-excursion
        (goto-char (point-min))
        (while (outline-next-heading)
          (let* ((old-id (plist-get (org-element--get-node-properties) :CUSTOM_ID))
                 (heading (replace-regexp-in-string "[^A-Za-z0-9]" "-" (strip-text-properties (org-get-heading t t t t))))
                 (new-id (concat "h-" heading))
                 (dup (assoc heading hlist))
                 (dup-count (if dup (1+ (cdr dup)) 1)))
            (setq new-id (concat new-id (if (= dup-count 1) "" (number-to-string dup-count))))
            (unless (string-equal old-id new-id)
              (org-set-property "CUSTOM_ID" new-id))
            (setq hlist (delete dup hlist))
            (push `(,heading . ,dup-count) hlist))))))
  (fset 'org-dedent-properties
        (kmacro-lambda-form
         [?\C-s ?: ?P ?R ?O ?P ?E ?R ?T ?I ?E ?S ?: return
                ?\C-a ?\C-x ? ?\C-s ?: ?E ?N ?D ?: return
                ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b
                134217848 ?k ?i ?l ?l ?- ?r ?e ?c ?t ?a ?n ?g ?l ?e return] 0 "%d"))
  (add-to-list 'org-structure-template-alist '("el" . "#+BEGIN_SRC emacs-lisp :tangle yes?\n\n#+END_SRC"))

  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-pretty-entities t

        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "** TODO %?\n  %i\n  %a")
          ("l" "Link" entry (file+headline "~/notes.org" "Links")
           "** %T %^L \n%?"))))

;;;   end


;;;   PlantUML
(leaf plantuml-mode :ensure t
  :init
  (setq plantuml-jar-path "~/Downloads/plantuml.jar"))
;;;   end


;;;   Snippet completion
(leaf yasnippet :ensure t
  :hook (prog-mode-hook . yas-minor-mode))
(leaf yasnippet-snippets :ensure t)
;;;   end


;;;   Syntax checking
(leaf flycheck :ensure t)
;;;   end

(defmacro p (form)
  "Output pretty `macroexpand-1'ed form of given FORM."
  `(progn
     (pp (macroexpand-1 ',form))
     nil))


;;;   Typescript support
(leaf typescript-mode :ensure t)
;;;   end


;;;   Better M-< and M->
(leaf beginend :ensure t
  :config
  (beginend-global-mode))
;;;   end


;;;   Manage services from Emacs
(leaf prodigy :ensure t
  :bind ("C-c C-p" . prodigy)
  :config
  (prodigy-define-service
    :name "NIT Durgapur backend"
    :command "pipenv"
    :args '("run" "python" "manage.py" "runserver" "0.0.0.0:8000")
    :cwd "~/Downloads/github.com/lugnitdgp/nitdgp_website/backend"
    :tags '(college django python)
    :stop-signal 'sigint
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "NIT Durgapur frontend"
    :command "npm"
    :args '("run" "dev")
    :cwd "~/Downloads/github.com/lugnitdgp/nitdgp_website/frontend"
    :tags '(college node js vue)
    :stop-signal 'sigint
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "NIT Durgapur new frontend"
    :command "yarn"
    :args '("serve")
    :cwd "~/Downloads/github.com/lugnitdgp/nitdgp-website-new"
    :tags '(college node js vue)
    :stop-signal 'sigint
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "Start Mariadb"
    :sudo t
    :command "systemctl"
    :args '("start" "mariadb.service")
    :tags '(system db)
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "Stop Mariadb"
    :sudo t
    :command "systemctl"
    :args '("stop" "mariadb.service")
    :tags '(system db)
    :kill-process-buffer-on-stop t))
;;;   end


;;;   Increase and decrease font size in Emacs (C-M-= and C-M--)
(leaf default-text-scale :ensure t
  :config (default-text-scale-mode 1))
;;;   end


;;;   Better buffer jumping (C-x C-b)
(leaf frog-jump-buffer :ensure t
  :init (require 'projectile) (projectile-mode 1)
  :bind ("C-x C-b" . frog-jump-buffer))
;;;   end


;;;   EXWM - Emacs Window Manager
(leaf exwm :ensure t :require t :leaf-defer nil
  :disabled t
  :init
  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-systemtray)
  (setq exwm-input-global-keys `(,(kbd "s-&") .
                                 (lambda (command)
                                   (interactive (list (read-shell-command "$ ")))
                                   (start-process-shell-command command nil command))))
  (exwm-systemtray-enable)
  (exwm-config-default)
  (ido-mode 0)
  (setenv "DESKTOP_SESSION" "exwm")
  (setenv "XDG_CURRENT_DESKTOP" "exwm")
  (setenv "XDG_SESSION_DESKTOP" "exwm"))
;;;   end


;;;   Mail search using notmuch
(leaf notmuch :ensure t
  :bind ((notmuch-search-mode-map
          ("d" . compro/notmuch/tag-as-deleted)
          ("<delchar>" . compro/notmuch/tag-as-deleted)
          ("u" . compro/notmuch/remove-deleted-tag)
          ("D" . compro/notmuch/remove-deleted-tag)
          ("f" . compro/notmuch/tag-as-flagged)
          ("F" . compro/notmuch/remove-flagged-tag)))
  :hook (message-mode-hook . notmuch-company-setup)
  :init
  (fset 'compro/notmuch/tag-as-deleted
        (kmacro-lambda-form [?+ ?d ?e ?l ?e ?t ?e ?d return] 0 "%d"))
  (fset 'compro/notmuch/remove-deleted-tag
        (kmacro-lambda-form [?- ?d ?e ?l ?e ?t ?e ?d return] 0 "%d"))
  (fset 'compro/notmuch/tag-as-flagged
        (kmacro-lambda-form [?+ ?f ?l ?a ?g ?g ?e ?d return] 0 "%d"))
  (fset 'compro/notmuch/remove-flagged-tag
        (kmacro-lambda-form [?- ?f ?l ?a ?g ?g ?e ?d return] 0 "%d")))
;;;   end


;;;   Different background colors for special buffers like file tree
(leaf solaire-mode :ensure t
  :hook
  (((ediff-prepare-buffer-hook
     treemacs-mode-hook
     magit-mode-hook
     terminal-mode-hook
     shell-mode-hook
     eshell-mode-hook) . solaire-mode)
   (minibuffer-setup-hook . solaire-mode-in-minibuffer)))
;;;   end


;;;   Eldoc(Function args help) at the point. Toggle using (C-c e).
(leaf pos-tip :ensure t :after eldoc :require t
  :config
  (defun compro/eldoc/display-message-momentary (format-string &rest args)
    "Display eldoc message near point."
    (when format-string
      (pos-tip-show (apply 'format format-string args) nil nil nil 0)))
  (global-set-key (kbd "C-c e")
                  (lambda ()
                    (interactive)
                    (setq eldoc-message-function
                          (if (eq eldoc-message-function 'ignore)
                              #'compro/eldoc/display-message-momentary
                            #'ignore)))))
;;;   end


;;;   Navbar(like Bootstrap Navbar)
(leaf navbar :require t :leaf-defer t :disabled t
  :load-path `,(concat user-emacs-directory ".repos/navbar.el")
  :config
  (defun get-exwm-buffers ()
    (let ((str ""))
      (mapcar
       (lambda (buffer)
         (let* ((buf-name (propertize (concat (buffer-name buffer) " x")
                                      'mouse-face 'highlight))
                (cross-start (length (buffer-name buffer)))
                (cross-end (length buf-name))
                (select-map (make-sparse-keymap))
                (close-map (make-sparse-keymap)))
           (define-key close-map [mouse-1]
             `(lambda (event)
                (interactive "e")
                (kill-buffer ,buffer)))
           (define-key select-map [mouse-1]
             `(lambda (event)
                (interactive "e")
                (call-interactively 'other-window)
                (sit-for 0.1)
                (exwm-workspace-switch-to-buffer ,buffer)))
           (add-text-properties
            0 cross-start
            (list
             'help-echo (concat "Switch to " (buffer-name buffer))
             'keymap select-map)
            buf-name)
           (add-text-properties
            (1+ cross-start) cross-end
            (list
             'help-echo (concat "Kill " (buffer-name buffer))
             'keymap close-map
             'face '((t (:foreground "#0ff")))
             'mouse-face '((t (:background "#f00"))))
            buf-name)
           (setq str (concat str buf-name " | "))))
       (seq-filter
        (lambda (buffer)
          (with-current-buffer buffer
            (eq major-mode 'exwm-mode)))
        (buffer-list)))
      str))
  (setq navbar-item-list '(get-exwm-buffers))
  (setq navbar-update-timer
        (run-with-timer 1.1 1.4 'navbar-sync))
  (navbar-mode)
  )
;;;   end


;;;   snails - A simple and modern completion framework
(leaf snails :leaf-defer nil :require t :disabled t
  :load-path "~/.emacs.d/.repos/snails")


;;;   Tabs in Emacs
(leaf centaur-tabs :leaf-defer nil :require t :disabled t
  :load-path `,(concat user-emacs-directory ".repos/centaur-tabs")
  :bind (("<C-M-S-iso-lefttab>" . centaur-tabs-forward-group)
         ("<C-M-tab>" . centaur-tabs-backward-group)
         ("C-c b" . centaur-tabs-counsel-switch-group))
  :preface
  (leaf powerline :ensure t)
  (defun compro/centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (and
       (not
        (or
         ;; Whitelist - Will not be hidden
         (string-equal "*Messages*" name)
         (string-equal "*scratch*" name)
         (string-equal "*Help*" name)
         (string-prefix-p "*eww" name)
         (string-prefix-p "*terminal" name)))
       (or
        ;; Current window is not dedicated window.
        (window-dedicated-p (selected-window))

        ;; Blacklist - Will be hidden

        ;; Is not magit buffer.
        (and (string-prefix-p "magit" name)
             (not (file-name-extension name)))
        ))))
  (defun compro/centaur-tabs-buffer-groups ()
    (let ((project-root (projectile-project-p))
          (project-name (projectile-project-name))
          (buf (buffer-name)))
      (cond
       ((or (string-prefix-p "*sent mail" buf)
            (string-prefix-p "*notmuch" buf)
            (string-equal "*unsent mail*" buf))
        '("Mail"))
       ((or (eq major-mode 'telega-chat-mode)
            (eq major-mode 'telega-root-mode))
        '("Telegram"))
       ((eq major-mode 'helpful-mode)
        '("Helpful"))
       ((or (eq major-mode 'eshell-mode)
            (eq major-mode 'term-mode))
        '("Terminals"))
       ((or (buffer-file-name)
            (eq major-mode 'dired-mode))
        (if (null project-root)
            '("Files")
          (list (concat "Project: " project-name) "Files")))
       ((string-prefix-p "*" buf)
        '("Internal"))
       (t '("Unregistered")))))
  :custom-face
  ((centaur-tabs-close-mouse-face . '((default (:foreground "orange red")))))
  :custom
  ((centaur-tabs-mouse-pointer . 'arrow)
   (centaur-tabs-style . "slant")  ;; slant, box, bar, wave, chamfer
   (centaur-tabs-height . 25)
   (centaur-tabs-set-icons . t)
   (centaur-tabs-set-bar . t)
   (centaur-tabs-cycle-scope . 'tabs)
   (centaur-tabs-set-modified-marker . t)
   (centaur-tabs-hide-tab-function . 'compro/centaur-tabs-hide-tab)
   (centaur-tabs-buffer-groups-function . 'compro/centaur-tabs-buffer-groups))
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  :bind
  (([C-tab] . centaur-tabs-forward)
   ([C-S-tab] . centaur-tabs-backward)
   ([C-S-iso-lefttab] . centaur-tabs-backward)))
;;;   end


;;;   Dired customizations
(leaf dired-x
  :bind ("C-x <C-j>" . dired-jump))
(leaf dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :bind ((dired-mode-map
          ("q"      . kill-current-buffer)
          ("RET"    . compro/dired-open-dir)
          ("^"      . compro/dired-up-dir)
          ("DEL"    . compro/dired-up-dir)
          ("<left>" . compro/dired-up-dir)))
  :preface

  (defun compro/dired-up-dir ()
    (interactive)
    (find-alternate-file ".."))

  (defun compro/dired-open-dir ()
    (interactive)
    (set-buffer-modified-p nil)
    (let ((file-or-dir (dired-get-file-for-visit)))
      (if (f-dir-p file-or-dir)
          (find-alternate-file file-or-dir)
        (find-file file-or-dir))))

  (defun compro/dired/mp3-to-ogg ()
    "Used in dired to convert mp3 files to ogg"
    (interactive)
    (let* ((files (dired-get-marked-files)))
      (dolist (file files)
        (let* ((basename (file-name-nondirectory file))
               (file-base (file-name-base file))
               (dirname (file-name-directory file))
               (extension (file-name-extension file))
               (ogg-file (concat dirname file-base ".ogg"))
               (command (format "mpg123 -s -v \"%s\" | oggenc --raw -o \"%s\" -" file ogg-file)))
          (if (string= "mp3" (downcase extension))
              (progn
                (shell-command command nil nil)
                (message command)
                (if (file-exists-p ogg-file)
                    (delete-file file))))))))

  :custom ((dired-dwim-target . t)))
;;;   end


;;;   elfeed - Reading articles(news feed) in Emacs
(leaf elfeed :ensure t
  :bind ("C-c w" . elfeed)
  :config
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "http://planet.emacsen.org/atom.xml"
          "http://emacshorrors.com/feed.atom")))
;;;   end


;;;   writeroom-mode - Center align a buffer and remove any distractions
(leaf writeroom-mode :ensure t
  :bind ("M-SPC" . writeroom-mode))
;;;   end


;;;   Assign specific positions for specific buffers
(leaf shackle :ensure t :require t
  :config
  (setq shackle-default-rule '(:select t))
  (setq shackle-rules
        '((help-mode :size 0.33 :select t :align bottom)))
  (shackle-mode 1))
;;;   end


;;;   hercules.el - A which-key based hydra
(leaf hercules :require t :leaf-defer nil :ensure t)
;;;   end


;;;   libvterm integration - Requires Emacs module support
(leaf vterm
  :load-path `,(concat user-emacs-directory ".repos/emacs-libvterm/")
  :bind (("C-`" . vterm)
         ("<C-M-return>" . vterm))
  :hook (vterm-exit-functions . (lambda (buf) (when buf (kill-buffer buf)))))
;;;   end
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" default))
 '(dired-dwim-target t t)
 '(org-agenda-diary-file "~/.org/diary.org" t)
 '(org-babel-load-languages '((emacs-lisp . t) (python . t)) t)
 '(org-return-follows-link t t)
 '(package-selected-packages
   '(shackle writeroom-mode elfeed solaire-mode notmuch exwm frog-jump-buffer default-text-scale prodigy beginend typescript-mode flycheck yasnippet-snippets yasnippet plantuml-mode org-re-reveal org-plus-contrib rustic system-packages cmake-mode elf-mode evil-collection evil impatient-mode http emmet-mode web-mode counsel swiper company-prescient ivy-prescient prescient ivy-posframe ivy-rich ivy pony-mode pipenv pyvenv py-autopep8 ccls lsp-python-ms lsp-ui company-lsp lsp-mode company-quickhelp page-break-lines vscode-icon dired-sidebar lsp-treemacs treemacs visual-fill-column doom-modeline chocolate-theme kaolin-themes doom-themes wgrep move-text undo-tree phi-search-mc multiple-cursors which-key switch-window ag projectile expand-region git-messenger forge keycast minions hungry-delete leaf))
 '(projectile-mode t nil (projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
