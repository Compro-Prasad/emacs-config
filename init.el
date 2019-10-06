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
(when (file-readable-p "~/.emacs.d/emacs-internals.el")
  ;; Install general for customizing keybinds
  (leaf general :ensure t)
  (leaf f :ensure t
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
  (leaf forge :after magit :require t :leaf-defer nil)
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
(leaf expand-region :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C-+" . er/contract-region)))
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
(leaf doom-themes :ensure t :require t :leaf-defer nil
  :config (load-theme 'doom-Iosvkem))
(leaf kaolin-themes :ensure t)
(leaf chocolate-theme :ensure t)
(leaf doom-modeline :ensure t
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-to-project))
;;;   end


;;;   Telegram in Emacs
(leaf telega
  :load-path "~/.emacs.d/.repos/telega.el"
  :bind ("C-c t" . telega)
  :preface
  (leaf visual-fill-column :ensure t)
  :config
  (setq telega-chat-use-markdown-formatting t))
;;;   end


;;;   Sidebar
(leaf treemacs :ensure t
  :bind (("<f9> t" . treemacs)
         (treemacs-mode-map
          ([mouse-1] . treemacs-single-click-expand-action))))
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
  (ivy-posframe-mode 0))
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
(leaf http :ensure t
  :bind
  (("<f9> h e" . httpd-start)
   ("<f9> h d" . httpd-stop)))
(leaf impatient-mode :ensure t
  :bind ("<f9> i" . impatient-mode))
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
(leaf exwm :ensure t :require t :leaf-defer nil :disabled t
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
  (ido-mode 0))
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
  :load-path "~/.emacs.d/.repos/navbar.el"
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


;;;   Tabs in Emacs
(leaf centaur-tabs :leaf-defer nil :require t
  :load-path "~/.emacs.d/.repos/centaur-tabs"
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


;;;   libvterm integration - Requires Emacs module support
(leaf vterm
  :load-path "~/.emacs.d/.repos/emacs-libvterm/"
  :commands (vterm)
  :hook (vterm-exit-functions . (lambda (buf) (when buf (kill-buffer buf)))))
;;;   end
