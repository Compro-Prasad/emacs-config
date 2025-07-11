;;; init.el ---                                      -*- lexical-binding: t; -*-

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

(require 'seq)
(setq is-windows
      (seq-find
       (lambda (x) (string= system-type x))
       '("ms-dos" "windows-nt" "cygwin")))
(setq is-unix
      (seq-find
       (lambda (x) (string= system-type x))
       '("gnu" "gnu/linux" "gnu/kfreebsd" "darwin" "cygwin")))
(setq is-gnu
      (seq-find
       (lambda (x) (string= system-type x))
       '("gnu" "gnu/linux" "gnu/kfreebsd")))
(setq is-linux
      (or
       (string= system-type "gnu")
       (string= system-type "gnu/linux")))
(setq is-mac (string= system-type "darwin"))
(setq is-bsd
      (or
       (string= system-type "gnu/kfreebsd")
       (string= system-type "darwin")))

(defun tangle-README.org-to-init.el ()
  "Tangle README.org to init.el"
  (let ((readme (ft (concat emacs-d "README.org")))
        (current-file (ft (buffer-file-name))))
    (when (string= readme current-file)
      (call-interactively 'org-babel-tangle))))

(defun early-init ()
  "Return `early-init.el' if greater than Emacs 27.
Else it will return `init.el'. Useful for tangling source code."
  (if (< emacs-major-version 27)
      "init.el"
    "early-init.el"))

(add-hook 'after-save-hook 'tangle-README.org-to-init.el)

(require 'package)

(defvar sslp (and (not (memq system-type '(windows-nt ms-dos)))
                  (gnutls-available-p))
  "Tells if SSL is enabled or not.")

(defvar protocol (if sslp "https" "http")
  "Protocol value as string.")

(defun compro/add-package-list (name url)
  "Add NAME and URL to `package-archives'.

URL should not have http:// or https:// as a prefix."
  (setf (alist-get name package-archives nil nil 'string=) (concat protocol "://" url)))

(compro/add-package-list "elpa-devel" "elpa.gnu.org/devel/")
(compro/add-package-list "melpa" "melpa.org/packages/")
(compro/add-package-list "nongnu" "elpa.nongnu.org/nongnu/")

(package-initialize)

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(use-package f :ensure t)
(use-package s :ensure t)

(defun mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values.

If there are multiple properties with the same keyword, only the first property
and its values are removed."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

(defun set-default-font (plists)
  "Set the font given the passed PLISTS.

PLISTS has either the form (\"fontname\" :prop1 val1 :prop2 val2 ...)
or is a list of such. The first font that can be found will be used.

The return value is nil if no font was found, truthy otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
    (dolist (plist plists)
      (when (find-font (font-spec :name (car plist)))
        (let* ((font (car plist))
               (props (cdr plist))
               (font-props (mplist-remove
                            ;; although this keyword does not exist anymore
                            ;; we keep it for backward compatibility
                            (mplist-remove props :powerline-scale)
                            :powerline-offset))
               (fontspec (apply 'font-spec :name font font-props)))
          (set-frame-font fontspec nil t)
          (push `(font . ,(frame-parameter nil 'font)) default-frame-alist)
          (pcase system-type
            (`gnu/linux
             (setq fallback-font-name "NanumGothic")
             (setq fallback-font-name2 "NanumGothic"))
            (`darwin
             (setq fallback-font-name "Arial Unicode MS")
             (setq fallback-font-name2 "Arial Unicode MS"))
            (`windows-nt
             (setq fallback-font-name "MS Gothic")
             (setq fallback-font-name2 "Lucida Sans Unicode"))
            (`cygwin
             (setq fallback-font-name "MS Gothic")
             (setq fallback-font-name2 "Lucida Sans Unicode"))
            (other
             (setq fallback-font-name nil)
             (setq fallback-font-name2 nil)))
          (when (and fallback-font-name fallback-font-name2)
            ;; remove any size or height properties in order to be able to
            ;; scale the fallback fonts with the default one (for zoom-in/out
            ;; for instance)
            (let* ((fallback-props (mplist-remove
                                    (mplist-remove font-props :size)
                                    :height))
                   (fallback-spec (apply 'font-spec
                                         :name fallback-font-name
                                         fallback-props))
                   (fallback-spec2 (apply 'font-spec
                                          :name fallback-font-name2
                                          fallback-props)))
              ;; window numbers
              (set-fontset-font "fontset-default"
                                '(#x2776 . #x2793) fallback-spec nil 'prepend)
              ;; mode-line circled letters
              (set-fontset-font "fontset-default"
                                '(#x24b6 . #x24fe) fallback-spec nil 'prepend)
              ;; mode-line additional characters
              (set-fontset-font "fontset-default"
                                '(#x2295 . #x22a1) fallback-spec nil 'prepend)
              ;; new version lighter
              (set-fontset-font "fontset-default"
                                '(#x2190 . #x2200) fallback-spec2 nil 'prepend))))
        (throw 'break t)))
    nil))

(defun compro/comint/kill-word (arg)
  (interactive "p")
  (unless buffer-read-only
    (let ((beg (point))
          (end (save-excursion (forward-word arg) (point)))
          (point (save-excursion (goto-char
                                  (if (> arg 0)
                                      (next-single-char-property-change
                                       (point) 'read-only)
                                    (previous-single-char-property-change
                                     (point) 'read-only)))
                                 (point))))
      (unless (get-char-property (point) 'read-only)
        (if (if (> arg 0) (< point end) (> point end))
            (kill-region beg point)
          (kill-region beg end))))))

(defun compro/comint/preoutput-read-only (text)
  (propertize text 'read-only t))

(defun compro/shell-kill-buffer-sentinel (process event)
  (when (and (memq (process-status process) '(exit signal))
             (buffer-live-p (process-buffer process)))
    (kill-buffer)))

(defun compro/kill-process-buffer-on-exit ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'compro/shell-kill-buffer-sentinel))

(dolist (hook '(ielm-mode-hook term-exec-hook comint-exec-hook))
  (add-hook hook 'compro/kill-process-buffer-on-exit))

(defun compro/get-empty-pkgs ()
  "Get 0 bytes .el packages."
  (let ((default-directory package-user-dir))
    (seq-reduce
     (lambda (value-list file)
       (if (= (file-attribute-size (file-attributes file)) 0)
           (cons file value-list)
         value-list))
     (seq-filter
      (apply-partially #'s-suffix-p ".el")
      (seq-reduce
       (lambda (value-list file)
         (if (and
              (not (s-prefix-p "." file))
              (file-accessible-directory-p file))
             (append
              (seq-map
               (apply-partially #'concat file "/")
               (directory-files file))
              value-list)
           value-list))
       (directory-files "")
       '()))
     '())))

(defun compro/redownload-empty-pkgs ()
  "Redownload empty packages."
  (interactive)
  (let* ((pkgs (compro/get-empty-pkgs))
         (default-directory package-user-dir)
         (choice-list (list
                       (cons (intern "Delete and re-download all") 1)
                       (cons (intern "Manually select for re-downloading") 2)
                       (cons (intern "Fix everything manually") 3)))
         (choice (if pkgs
                     (alist-get
                      (intern
                       (completing-read
                        (concat
                         "Some files were not properly downloaded namely "
                         (s-join ", " pkgs)
                         ". What action do you want to take?  ")
                        choice-list))
                      choice-list)
                   3)))
    (if (= choice 3)
        (when (null pkgs)
          (message "No empty packages were found"))
      (package-refresh-contents)
      (seq-each
       (lambda (file)
         (let* ((values (s-split "/" file))
                (dir-name (car values))
                (pkg-values (s-split "-" dir-name))
                (pkg-name (s-join "-" (butlast pkg-values 1)))
                (each-choice
                 (if (= choice 1)
                     t
                   (yes-or-no-p
                    (concat "Delete and re-download " dir-name "? ")))))
           (when each-choice
             (delete-directory dir-name t)
             (ignore-errors
               (package-reinstall (intern pkg-name))))))
       pkgs))))

(defun re-download (pkg &optional arg)
  "Advice for package-install."
  (let* ((pkg-name (symbol-name (if (package-desc-p pkg)
                                    (package-desc-name pkg)
                                  pkg)))
         (file-name (car
                     (sort
                      (seq-filter
                       (apply-partially #'s-prefix-p pkg-name)
                       (compro/get-empty-pkgs))
                      #'string-greaterp)))
         (dir (when file-name (car (s-split "/" file-name)))))
    (when dir
      (delete-directory dir)
      (ignore-errors (package-reinstall pkg)))))
(advice-add 'package-install :after 're-download)

(defun switch-to-buffer-current-major-mode ()
  "Switch to buffer like functionality based on current major mode."
  (interactive)
  (let* ((m-mode major-mode)
         (prompt (concat (symbol-name m-mode) " buffers: ")))
    (read-buffer
     prompt nil (confirm-nonexistent-file-or-buffer)
     (lambda (buf)
       (with-current-buffer (cdr buf)
         (eq m-mode major-mode))))))

(global-set-key (kbd "C-x C-b") 'switch-to-buffer-current-major-mode)

(setq compro/laptop-p (equal system-name "hp-archlinux"))

(use-package general :ensure t)

(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)

(use-package tab-bar
  :when (> emacs-major-version 27)
  :bind (("C-t" . tab-bar-new-tab-event)
         ([C-f4] . tab-bar-close-tab)
         ("C-S-t" . tab-bar-undo-close-tab)
         ([C-tab] . tab-next)
         ([C-backtab] . tab-previous)
         ([C-S-tab] . tab-previous)
         ([C-iso-lefttab] . tab-previous))
  :init
  (defun switch-to-untitled-buffer ()
    (interactive)
    (let ((buf (format "untitled-%d" (random 100000))))
      (generate-new-buffer buf)
      (switch-to-buffer buf)
      (setq buffer-offer-save 'always)))

  (defvar tab-bar-new-commands
    '((?p "Project" project-switch-project)
      (?n "New buffer" switch-to-untitled-buffer)
      (?f "List Files" find-file)
      (?b "List Buffers" switch-to-buffer)
      (?r "Run command" execute-extended-command)
      (?q "Do nothing" ignore)))
  (defun tab-bar-new--keymap-prompt ()
    "Return a prompt for the project swithing dispatch menu."
    (mapconcat
     (pcase-lambda (`(,key ,label))
       (format "%s %s"
               (propertize
                (key-description `(,(concat " " (char-to-string key) " ")))
                'face '(:foreground "black" :background "cyan" :weight bold))
               label))
     tab-bar-new-commands
     "  "))
  (defun tab-bar-new-tab-event ()
    (interactive)
    (when-let ((choice (assq (read-event (tab-bar-new--keymap-prompt))
                             tab-bar-new-commands))
               (inhibit-quit t))
      (tab-bar-new-tab)
      (when (not (char-equal (nth 0 choice) ?q))
        (switch-to-buffer "waiting...")
        (insert "Churning data or waiting for IO")
        (with-local-quit (call-interactively (nth 2 choice)))
        (kill-buffer "waiting..."))
      (message "New tab created with `%s' option" (nth 1 choice))))

  :config
  (setq tab-bar-format
        '(tab-bar-format-history
          tab-bar-separator tab-bar-separator
          tab-bar-format-tabs
          tab-bar-separator tab-bar-separator tab-bar-separator
          tab-bar-format-add-tab
          tab-bar-separator tab-bar-separator tab-bar-separator
          tab-bar-format-global))
  (tab-bar-mode))

(use-package dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("C-c C-c" . dired-collapse-mode)
              ("C-c C-d C-u" . dired-du-mode)
              ("." . dired-hide-dotfiles-mode)
              ("<tab>" . dired-subtree-toggle)
              ("q"      . kill-current-buffer)
              ("RET"    . compro/dired-open-dir)
              ("^"      . compro/dired-up-dir)
              ("DEL"    . compro/dired-up-dir)
              ("<left>" . compro/dired-up-dir)
              ("C-x <C-j>" . dired-jump))
  :init
  (use-package dired-collapse :ensure t)
  (use-package dired-du :ensure t :after dired)
  (use-package dired-dups :ensure t :after dired)
  (use-package dired-filetype-face :ensure t :after dired)
  (use-package dired-hide-dotfiles :ensure t
    :after dired
    :hook (dired-mode-hook . dired-hide-dotfiles-mode))
  (use-package dired-subtree :ensure t :after dired)
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

  :config
  (setq dired-dwim-target t)
  (defun mydired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))

  (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (mydired-sort)))

(when compro/laptop-p
  (setq user-mail-address "comproprasad@gmail.com"
        user-full-name "Compro Prasad"))

(setq-default
 ;;;   Use spaces and not tabs for indentation
 indent-tabs-mode nil

 ;;;   Don't highlight trailing whitespaces by default
 show-trailing-whitespace nil

 ;;;   Org
 org-src-fontify-natively t ;; Fontify source blocks

 ;;;   More number of characters on a single line
 fill-column 80
 )

(setq
 ;;;   Load newer files
 load-prefer-newer t

 ;;;   Initial major mode for *scratch* buffer
 initial-major-mode 'fundamental-mode

 ;;;   Only use ~/.authinfo.gpg
 auth-sources (list (ft "~/.authinfo.gpg"))

 ;;;   Security settings
 gnutls-verify-error t

 ;;;   Customizations go to this file
 custom-file (expand-file-name "custom.el" cache-d)

 ;;;   Follow symlinks to the actual file
 find-file-visit-truename t
 vc-follow-symlinks t

 ;;;   Don't redisplay if input is in buffer. Makes scrolling smoother.
 redisplay-skip-fontification-on-input t

 ;;;   Jump by words separated by punctuations
 global-subword-mode t

 ;;;   Prompt GNUPG passwords in the minibuffer only
 epg-pinentry-mode 'loopback

 ;;;   Show keystrokes in minibuffer after 0.5 seconds
 echo-keystrokes 0.5

 ;;;   Turn on every disabled function
 disabled-command-function nil

 ;;;   Use UTF-8 characters in buffer
 buffer-file-coding-system 'utf-8

 ;;;   Disable bidirectional text for tiny performance boost
 bidi-display-reordering nil

 ;;;   Don't blink parens
 blink-matching-paren nil

 ;;;   Hide cursors in other windows
 cursor-in-non-selected-windows nil

 ;;;   Prevent frames from automatically resizing themselves
 frame-inhibit-implied-resize t

 ;;;   Clipboard length
 kill-ring-max 1024

 ;;;   Stretch cursor according to the character under it
 x-stretch-cursor t

 ;;;   Time to wait before start of stealth fontify
 jit-lock-stealth-time 120

 ;;;   Sentences are separated by single space after dot(.)
 sentence-end-double-space nil

 ;;;   Don't compact font cache during GC to optimize redisplay
 inhibit-compacting-font-caches t

 ;;;   GC triggers per 100 MB increase in memory
 gc-cons-threshold (* 100 1024 1024)
 gc-cons-threshold-bak gc-cons-threshold  ;; Backup

 ;;;   Increase buffer size for reading output of processes (5 MB)
 read-process-output-max (* 5 1024 1024)

 ;;;   Prevent recursion limits
 max-lisp-eval-depth 700
 max-specpdl-size 700

 ;;;   No bells
 ring-bell-function 'ignore
 visible-bell nil

 ;;;   Themes are safe after all
 custom-safe-themes t

 ;;;   No startup show off
 inhibit-startup-screen t

 ;;;   Show line number for any normal width line
 line-number-display-limit-width 10000000

 ;;;   Some TLS connections might have larger PRIME bits
 gnutls-min-prime-bits 4096

 ;;;   Better unique names of similar filenames and buffer-names
 uniquify-buffer-name-style 'forward

 ;;;   We can use TCP connection to connect to remote Emacs instance
 server-use-tcp t

 ;;;   Server location
 server-auth-dir (concat cache-d "server/")

 ;;;   Save existing interprogram clipboard text before replacing it
 save-interprogram-paste-before-kill t

 ;;;   Set REPL programs' prompt as read only
 comint-prompt-read-only t

 ;;;   Read more output from a process (2mb)
 read-process-output-max 2097152

 ;;;   Use commands when in in minibuffer
 enable-recursive-minibuffers t

 ;;;   Scroll one line at a time no matter what
 scroll-conservatively  10000

 ;;;   Increase update time
 idle-update-delay 1.0

 ;;;   Initial scratch message is nil
 initial-scratch-message ""

 ;;;   Use directory local variables in tramp session
 enable-remote-dir-locals t

 ;;;   Backup configuration
 tramp-persistency-file-name (concat cache-d "tramp")
 backup-directory-alist `(("." . ,(concat cache-d "backups")))
 delete-old-versions -1
 version-control t
 vc-make-backup-files t
 vc-handled-backends '(Git)
 auto-save-file-name-transforms `((".*" ,(concat cache-d "auto-save-list") t))
 auto-save-list-file-prefix (concat cache-d "auto-save-list/saves-")

 ;;;   ERC configurations
 erc-hide-list '("PART" "QUIT" "JOIN")
 erc-server    "irc.libera.chat"
 erc-nick      "compro"

 ;;;   Dired
 dired-dwim-target t
 dired-listing-switches "-lAh --group-directories-first"

 ;;;   Ediff
 ediff-window-setup-function 'ediff-setup-windows-plain ;; Single frame ediff session

 ;;;   Ido mode
 ido-enable-flex-matching t
 ido-save-directory-list-file (concat cache-d "ido.last")

 ;;;   TAB cycle if there are only few candidates
 completion-cycle-threshold 5

 ;;;   Complete after indenting
 tab-always-indent 'complete

 ;;;   Increase interval at which eldoc is shown
 eldoc-idle-delay 1.5
 )

(if (>= emacs-major-version 28)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(when (file-readable-p custom-file)
  (load custom-file))

(when (file-readable-p "~/.git-tokens")
  (load-file "~/.git-tokens"))

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defun compro/unset-keys ()
  (general-define-key
   :keymaps 'input-decode-map
   [?\C-m] [C-m]
   [?\C-i] [C-i]
   [?\C-j] [C-j]
   [?\C-\[] (kbd "<C-[>"))
  (remove-hook 'server-after-make-frame-hook 'compro/unset-keys))

;; For daemon / server sessions
(add-hook 'server-after-make-frame-hook 'compro/unset-keys)

;; For non daemon / server sessions
(compro/unset-keys)

(general-define-key
 "C-z"             'undo
 "C-x C-o"         'ff-find-other-file
 [C-m]             'delete-other-windows
 "<C-S-mouse-1>"   'imenu
 "C-c r"           'imenu
 "M-/"             'hippie-expand
 "M-^"             'compile)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(if (< emacs-major-version 28)
    (global-set-key [mouse-3] menu-bar-edit-menu)
  (context-menu-mode 1))

(use-package autorevert
  :config
  (setq auto-revert-remote-files nil)
  (global-auto-revert-mode t))

(use-package paren
  :config
  (setq show-paren-style 'mixed
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode t))

(add-hook 'prog-mode-hook 'which-function-mode)

(add-hook 'prog-mode-hook 'electric-pair-mode)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize the compilation buffer with ANSI escape sequences."
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun compro/rename-file-buffer (&optional arg)
  "Rename current buffer and the file it is linked to.

If no prefix argument is provided simple string input is provided
using `read-string' function.

If a prefix argument (\\[universal-argument]) is provided full
featured `read-file-name' is used to read the filename. This is
useful if you want to move the file from one directory to another."
  (interactive "p")
  (when (null (buffer-file-name))
    (error "Buffer `%s' is not linked to a file" (buffer-name)))
  (let* ((filepath (buffer-file-name))
         (filename (f-filename filepath))
         (filedir (file-name-directory (directory-file-name filepath)))
         (prompt (concat "Rename '" filename "' to: "))
         (move-p (> arg 1))
         (new-location (if move-p
                           (read-file-name prompt filedir filepath)
                         (read-string prompt filename)))
         (new-filepath (if (string-suffix-p "/" new-location)
                           (concat new-location filename)
                         new-location)))
    (rename-file filename new-location 1)
    (set-visited-file-name new-filepath t t)))

(global-set-key (kbd "C-c f r") 'compro/rename-file-buffer)

(use-package simple
  :bind (("C-a" . compro/beginning-of-line)
         ("C-S-p" . list-processes)
         ("" . list-processes))
  :init
  (defun compro/beginning-of-line ()
    (interactive)
    (if (bolp)
        (back-to-indentation)
      (let ((pos (point))
            npos)
        (save-excursion
          (back-to-indentation)
          (setq npos (point)))
        (if (= pos npos)
            (beginning-of-line)
          (back-to-indentation))))))

(with-eval-after-load 'comint
  (general-define-key
   :kemaps 'comint-mode-map
   "<remap> <kill-word>" 'compro/comint/kill-word))

(add-hook 'comint-preoutput-filter-functions
          'compro/comint/preoutput-read-only)

(require 'savehist)
(setq history-length t
      history-delete-duplicates t
      savehist-file (concat cache-d "savehist")
      save-place-file (concat cache-d "saveplace")
      savehist-additional-variables (nconc savehist-additional-variables
                                           '(kill-ring
                                             extended-command-history
                                             global-mark-ring
                                             mark-ring
                                             regexp-search-ring
                                             search-ring)))
(save-place-mode 1)
(savehist-mode 1)

(require 'recentf)
(setq recentf-max-saved-items 512
      recentf-save-file (concat cache-d "recentf"))
(add-to-list 'recentf-exclude
             (concat (regexp-quote (ft (format cache-d))) ".*"))
(recentf-mode 1)

(use-package xwidget :when (fboundp 'xwidget-webkit-browse-url)
  :bind
  (:map xwidget-webkit-mode-map
        ("<mouse-4>" . xwidget-webkit-scroll-down)
        ("<mouse-5>" . xwidget-webkit-scroll-up)
        ("<up>" . xwidget-webkit-scroll-down)
        ("<down>" . xwidget-webkit-scroll-up)
        ("M-w" . xwidget-webkit-copy-selection-as-kill)
        ("C-c" . xwidget-webkit-copy-selection-as-kill))

  :hook
  (window-configuration-change-hook . compro/xwidget-webkit/adjust-size)

  :init
  ;; by default, xwidget reuses previous xwidget window,
  ;; thus overriding your current website, unless a prefix argument
  ;; is supplied
  ;; This function always opens a new website in a new window
  (defun xwidget-browse-url-no-reuse (url &optional session)
    (interactive
     (progn
       (require 'browse-url)
       (browse-url-interactive-arg "xwidget-webkit URL: ")))
    (xwidget-webkit-browse-url url t))

  (defun compro/xwidget-webkit/adjust-size ()
    (when (equal major-mode 'xwidget-webkit-mode)
      (xwidget-webkit-adjust-size-dispatch))))

(add-hook 'tabulated-list-mode-hook 'hl-line-mode)

(use-package winner :config (winner-mode 1))

(defun compro/set-show-whitespace-mode ()
  "Show white space in current buffer"
  (setq show-trailing-whitespace t))
;; Show whitespaces only in buffers pointing to specific files
(add-hook 'find-file-hook 'compro/set-show-whitespace-mode)
;; Remove the trailing whitespaces on save
(add-hook 'before-save-hook
          #'(lambda ()
              (when (not (eq major-mode 'org-mode))
                (delete-trailing-whitespace))))

(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold gc-cons-threshold-bak)
  (garbage-collect))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

(c-add-style "mylinux"
             '("linux"
               (tab-width . 4)
               (c-basic-offset . 4)
               (fill-column . 80)
               (c-hanging-semi&comma-criteria . my/c-semi&comma)
               (c-cleanup-list empty-defun-braces ;; {}
                               brace-else-brace   ;; } else {
                               brace-elseif-brace ;; } else if {
                               ;;defun-close-semi   ;; };
                               )
               (c-hanging-braces-alist (brace-list-open)
                                       (brace-entry-open)
                                       (substatement-open after)
                                       (block-close . c-snug-do-while)
                                       (arglist-cont-nonempty)
                                       (class-open . (after))
                                       (class-close . (before)))
               (c-offsets-alist (inline-open . 0)
                                (comment-intro . 0))))

(setq-default c-default-style
              '((java-mode . "java")
                (awk-mode . "awk")
                (other . "mylinux")))

(defun default-ansi-term-opener (dir)
  (let ((buf-name (funcall terminal-buffer-name-generator dir "ansi-term" nil))
        (*buf-name* (concat "*" buf-name "*")))
    (if (get-buffer *buf-name*)
        `(,*buf-name* . nil)
      (ansi-term "/bin/bash" buf-name)
      `(,*buf-name* . t))))

(defun default-eshell-opener (dir)
  (let ((eshell-buffer-name (funcall terminal-buffer-name-generator dir "eshell" t)))
    (if (get-buffer eshell-buffer-name)
        `(,eshell-buffer-name . nil)
      (eshell)
      `(,eshell-buffer-name . t))))

(defun default-vterm-opener (dir)
  (let ((buf-name (funcall terminal-buffer-name-generator dir "vterm" t)))
    (if (get-buffer buf-name)
        `(,buf-name . nil)
      (vterm buf-name)
      `(,buf-name . t))))

(defun default-terminal-buffer-name-generator (dir terminal-type use-star)
  (if use-star
      (concat "*" terminal-type "-" dir "*")
    (concat terminal-type "-" dir)))

(defun detect-terminal-and-open (dir)
  (if (featurep 'vterm)
      (default-vterm-opener dir)
    (prog1
        (default-eshell-opener dir)
      (message "Opened eshell because vterm was not installed"))))

(defun below-window-checker (buffer action)
  (with-current-buffer buffer
    (or
     (s-starts-with? "*eshell" buffer)
     (s-starts-with? "*vterm" buffer)
     (s-starts-with? "*ansi-term" buffer))))

(defvar terminal-buffer-name-generator 'default-terminal-buffer-name-generator)
(defvar terminal-generator 'detect-terminal-and-open)

(defun toggle-terminal ()
  "Toggles terminal like VS Code does.

TODO:

1. Creating new terminals in the same project for different purposes.
2. Editing path in which the terminal is opened. Currently best guess it taken.
3. Allow switching between different terminals in the same path with ease.
"
  (interactive)
  (let* ((prj (project-current))
         (dir (if prj
                  (expand-file-name (project-root prj))
                default-directory))
         (ret-val (let ((default-directory dir)) (funcall terminal-generator dir)))
         (created (cdr ret-val))
         (buf-name (car ret-val))
         (buf-obj (get-buffer buf-name))
         (buf-win (get-buffer-window buf-name nil)))

    (if buf-win
        (if (and (not created) (eq buf-win (selected-window)))
            (progn (message "Hiding terminal window") (delete-window buf-win))

          (progn (message "Selecting terminal window") (select-window buf-win t)))

      (message "Showing terminal buffer in a window")
      (display-buffer-in-side-window buf-obj '((side . bottom)))
      (select-window (get-buffer-window buf-obj) t))))

(defun create-new-terminal ()
  "WIP"
  (let* ((prj (project-current))
         (dir (if prj
                  (expand-file-name (project-root prj))
                default-directory))
         (ret-val (funcall terminal-generator dir))
         (created (cdr ret-val))
         (buf-name (car ret-val))
         (buf-obj (get-buffer buf-name))
         (buf-win (get-buffer-window buf-name nil)))

    (if buf-win
        (if (and (not created) (eq buf-win (selected-window)))
            (progn (message "Hiding terminal window") (delete-window buf-win))

          (progn (message "Selecting terminal window") (select-window buf-win t)))

      (message "Showing terminal buffer in a window")
      (display-buffer-in-side-window buf-obj '((side . bottom)))
      (select-window (get-buffer-window buf-obj) t))))

(add-to-list
 'display-buffer-alist
 '(below-window-checker
   display-buffer-in-side-window
   (side . bottom)
   ;; (slot . 1)
   ;; (window-width . 0.33)
   ;; (reusable-frames . nil)
   ))

(global-set-key (kbd "C-`") 'toggle-terminal)
(global-set-key (kbd "C-c `") 'toggle-terminal)

(use-package restclient :ensure t)

(use-package hydra :ensure t)

(global-set-key
 (kbd "C-c u")
 (defhydra hydra-ui (:hint nil)
   "
  ^Emacs^              ^Move to window^   ^Move window to^   ^Buffer^
  ^^^^-----------------------------------------------------------------------
  _M-+_: Inc font      _<left>_           _S-<left>_         _f_: Col indicator
  _M-=_: Inc font      _<right>_          _S-<right>_        _l_: Line numbers
  _M--_: Dec font      _<up>_             _S-<up>_           _+_: Inc font
  _F_: Col indicator   _<down>_           _S-<down>_         _=_: Inc font
  _L_: Line numbers    ^ ^                ^ ^                _-_: Dec font
  _t_: Tabs
  _T_: Toolbar
  _m_: Menubar
  _s_: Scrollbar"
   ("+" text-scale-increase)
   ("=" text-scale-increase)
   ("-" text-scale-decrease)
   ("M-+" default-text-scale-increase)
   ("M-=" default-text-scale-increase)
   ("M--" default-text-scale-decrease)
   ("t" tab-bar-mode)
   ("m" menu-bar-mode)
   ("s" scroll-bar-mode)
   ("f" display-fill-column-indicator-mode)
   ("l" display-line-numbers-mode)
   ("F" global-display-fill-column-indicator-mode)
   ("L" global-display-line-numbers-mode)
   ("T" tool-bar-mode)
   ("<left>" windmove-left)
   ("<right>" windmove-right)
   ("<up>" windmove-up)
   ("<down>" windmove-down)
   ("S-<left>" buf-move-left)
   ("S-<right>" buf-move-right)
   ("S-<up>" buf-move-up)
   ("S-<down>" buf-move-down)))

(global-set-key
 (kbd "C-c t")
 (defhydra hydra-text ()
   ("x" whole-line-or-region-kill-region "Cut")
   ("c" whole-line-or-region-kill-ring-save "Copy")
   ("v" yank "Paste")
   ("C-x" whole-line-or-region-kill-region "Cut")
   ("C-c" whole-line-or-region-kill-ring-save "Copy")
   ("C-v" yank "Paste")
   ("C" consult-yank-pop "Clipboard")
   ("<up>" previous-line nil)
   ("C-p" previous-line nil)
   ("<down>" next-line nil)
   ("C-n" next-line nil)
   ("<left>" left-char nil)
   ("<right>" right-char nil)
   ("C-<left>" left-word nil)
   ("M-b" backward-word nil)
   ("C-<right>" right-word nil)
   ("M-f" forward-word nil)
   ("s" avy-goto-char-2 "Goto 2 chars")
   ("S" avy-goto-symbol-1 "Goto symbol")
   ("C-s" ctrlf-forward-default "Find Next")
   ("C-f" ctrlf-forward-default "Find Next")
   ("C-r" ctrlf-backward-default "Find Previous")
   ("C-S-f" ctrlf-backward-default "Find Previous")
   ("<home>" compro/beginning-of-line nil)
   ("C-a" compro/beginning-of-line "Home")
   ("<end>" move-end-of-line nil)
   ("C-e" move-end-of-line "End")
   ("C-SPC" set-mark-command "Mark/Unmark")
   ("S-<down>" move-text-down "Move line down")
   ("S-<up>" move-text-up "Move line up")
   ("+" er/expand-region "Expand")
   ("=" er/expand-region "Expand")
   ("C-+" hydra-er/er/expand-region "Expand")
   ("C-=" hydra-er/er/expand-region "Expand")
   ("-" er/contract-region "Contract")
   ("C--" hydra-er/er/contract-region "Contract")))

(global-set-key
 (kbd "C-c g")
 (defhydra hydra-gamify (:hint nil)
   "Game mode"
   ("w" previous-line)
   ("s" next-line)
   ("W" previous-line)
   ("S" next-line)
   ("a" left-char)
   ("d" right-char)
   ("A" left-word)
   ("D" right-word)
   ("C-s" ctrlf-forward-default)
   ("C-r" ctrlf-backward-default)
   ("c" whole-line-or-region-kill-ring-save)
   ("x" whole-line-or-region-kill-region)
   ("v" yank)
   ("C-c" whole-line-or-region-kill-ring-save)
   ("C-x" whole-line-or-region-kill-region)
   ("C-v" yank)
   ("V" consult-yank-pop "Clipboard")
   ("g" set-mark-command "Mark")
   ("f" avy-goto-char-2 "Goto 2 chars")
   ("F" avy-goto-symbol-1 "Goto symbol")
   ("t" treemacs "Treemacs")
   ("<left>" windmove-left)
   ("<right>" windmove-right)
   ("<up>" windmove-up)
   ("<down>" windmove-down)
   ("S-<left>" buf-move-left)
   ("S-<right>" buf-move-right)
   ("S-<up>" buf-move-up)
   ("S-<down>" buf-move-down)
   ("j" windmove-left)
   ("l" windmove-right)
   ("i" windmove-up)
   ("j" windmove-down)
   ("J" buf-move-left)
   ("L" buf-move-right)
   ("I" buf-move-up)
   ("K" buf-move-down)
   ("u" undo)
   ("U" undo-tree-visualize)
   ("z" undo)
   ("Z" undo-tree-visualize)
   ("e" end-of-buffer)
   ("E" beginning-of-buffer)
   ("M-c" capitalize-word "Capitalize")
   ("M-l" downcase-word "Lower")
   ("M-u" upcase-word "Upper")
   ("o" compro/beginning-of-line)
   ("p" move-end-of-line)))

(use-package hungry-delete :ensure t
  :config (global-hungry-delete-mode t))

(if (>= emacs-major-version 31)
    (setq mode-line-collapse-minor-modes t)
  (use-package minions :ensure t
    :bind ([S-down-mouse-3] . minions-minor-modes-menu)
    :config
    (minions-mode 1)))

(use-package transient :ensure t
  :config
  (setq transient-history-file (locate-user-emacs-file
                                (concat cache-d "transient/history.el"))
        transient-values-file (locate-user-emacs-file
                               (concat cache-d "transient/values.el"))
        transient-levels-file (locate-user-emacs-file
                               (concat cache-d "transient/levels.el"))))

(use-package magit :ensure t
  :bind (("C-x g" . magit-status)
         :map magit-mode-map
         ([C-tab] . nil)
         ([C-backtab] . nil)
         ([M-tab] . nil)
         :map magit-status-mode-map
         ("q" . compro/kill-magit-buffers)
         ([C-tab] . nil)
         ([C-backtab] . nil)
         ([M-tab] . nil)
         :map magit-log-mode-map
         ([C-tab] . nil)
         ([C-backtab] . nil)
         ([M-tab] . nil))
  :init
  ;; (use-package forge :unless is-windows :after magit :ensure t)
  :config
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
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

(use-package git-messenger :ensure t
  :bind (("C-x v p" . git-messenger:popup-message)))

(use-package expand-region :ensure t
  :commands (er/expand-region
             er/mark-paragraph
             er/mark-inside-pairs
             er/mark-outside-pairs
             er/mark-inside-quotes
             er/mark-outside-quotes
             er/contract-region)
  :bind (("C-=" . hydra-er/er/expand-region)
         ("C--" . hydra-er/er/contract-region)
         ("M-[ 1 ; 5 k" . hydra-er/er/expand-region)  ; Strange key in git bash (msys2) on windows
         ("M-[ 1 ; 5 m" . hydra-er/er/contract-region))  ; Strange key in git bash (msys2) on windows
  :config
  (require 'hydra)
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

(use-package projectile :ensure t
  :unless (> emacs-major-version 27)  ;; Use project.el for > 27
  :bind (("C-x p" . projectile-command-map))
  :config
  (setq
   projectile-cache-file (concat cache-d "projectile")
   projectile-known-projects-file (concat cache-d "projectile-bookmarks.eld")
   projectile-completion-system 'default)
  (projectile-mode 1))

(use-package project-x
  :config
  (project-x-mode 1))

(use-package ag :ensure t :when (executable-find "ag"))

(use-package switch-window :ensure t
  :bind ("C-x o" . switch-window))

(use-package which-key :ensure t
  :config
  (setq which-key-idle-delay (if is-windows 0.212 1.0))
  (which-key-mode))

(use-package multiple-cursors :ensure t
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
  (use-package phi-search-mc :ensure t
    :hook (isearch-mode-hook . phi-search-from-isearch-mc/setup-keys)
    :config
    (phi-search-mc/setup-keys)))

(use-package vundo :ensure t
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package doom-themes :ensure t
  :commands (doom-themes-org-config)
  :config
  (doom-themes-org-config)
  ;; (setq doom-themes-enable-bold t     ;; Causes font-lock to slow down
  ;;       doom-themes-enable-italic t)
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

(use-package spacemacs-theme :ensure t)

(use-package modus-themes :ensure t)

(use-package page-break-lines :ensure t
  :config
  (global-page-break-lines-mode t))

(use-package orderless :ensure t
  :config
  (setq completion-styles '(orderless flex substring)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package consult :ensure t
  :bind (("M-y" . consult-yank-pop)
         ("M-v" . consult-yank-pop)
         ("C-v" . consult-yank-pop)
         ("M-g l" . consult-line)
         ("M-g o" . consult-outline)
         ("C-x C-r" . consult-recent-file)
         ("C-x b" . consult-buffer)
         :map minibuffer-local-map
         ("C-r" . consult-history)))

(use-package consult-dir :ensure t
  :bind ("C-x d" . consult-dir)
  :init
  (with-eval-after-load 'eshell
    (defun eshell/z (&optional regexp)
      "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'.
Source: https://karthinks.com/software/jumping-directories-in-eshell/"
      (let ((eshell-dirs (delete-dups
                          (mapcar 'abbreviate-file-name
                                  (ring-elements eshell-last-dir-ring)))))
        (cond
         ((and (not regexp) (featurep 'consult-dir))
          (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                     :narrow ?e
                                                     :category file
                                                     :face consult-file
                                                     :items ,eshell-dirs))
                 (consult-dir-sources (cons consult-dir--source-eshell
                                            consult-dir-sources)))
            (eshell/cd (substring-no-properties
                        (consult-dir--pick "Switch directory: ")))))
         (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                         (completing-read "cd: " eshell-dirs)))))))))

(use-package marginalia :ensure t :after vertico
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode +1))

(use-package embark :ensure t
  :bind (("C-S-a" . embark-act)
         ("" . embark-act)
         ("C-S-e" . embark-act-noexit)
         ("" . embark-act-noexit)
         ("C-S-b" . embark-become)
         ("" . embark-become))
  :config
  ;; which-key support
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package vertico :ensure t :defer nil

  ;; More convenient directory navigation commands
  :bind (:map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word)
          ("M-V" . vertico-multiform-vertical)
          ("M-G" . vertico-multiform-grid)
          ("M-F" . vertico-multiform-flat)
          ("M-R" . vertico-multiform-reverse)
          ("M-U" . vertico-multiform-unobtrusive))

  :init
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)

  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)

  :config
  (setq vertico-count 18)
  (require 'vertico-mouse)
  (require 'vertico-indexed)
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (vertico-mouse-mode 1)
  (vertico-indexed-mode 1)
  (advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                 "  ")
               cand)))
  ;; Selectrum Wiki - Minibuffer default add function
  (autoload 'ffap-guesser "ffap")
  (setq minibuffer-default-add-function
        (defun minibuffer-default-add-function+ ()
          (with-selected-window (minibuffer-selected-window)
            (delete-dups
             (delq nil
                   (list (thing-at-point 'symbol)
                         (thing-at-point 'list)
                         (ffap-guesser)
                         (thing-at-point-url-at-point))))))))

(use-package vertico-directory :after vertico :ensure nil)

(use-package cc-isearch-menu :ensure casual
  :config
  (define-key isearch-mode-map (kbd "<f2>") 'cc-isearch-menu-transient))

(use-package beginend :ensure t
  :config (beginend-global-mode))

(use-package move-text :ensure t)

(use-package default-text-scale :ensure t
  :config (default-text-scale-mode 1))

(use-package iedit :ensure t
  :bind ("C-c i" . iedit-mode))

(use-package wgrep :ensure t :after grep)

(use-package clang-format+ :ensure t
  :config
  (setq clang-format+-context 'buffer))

(use-package telega :ensure t :when is-linux)

(use-package org :ensure org-contrib
  :hook (org-mode-hook . org-superstar-mode)
  :init
  ;; see https://list.orgmode.org/87r5718ytv.fsf@sputnik.localhost
  (eval-after-load 'org-list
    '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

  (defun ndk/checkbox-list-complete ()
    (save-excursion
      (org-back-to-heading t)
      (let ((beg (point)) end)
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))

  (use-package casual-agenda
    ;; https://github.com/kickingvegas/casual-agenda
    :ensure t
    :bind (:map
           org-agenda-mode-map
           ("C-o" . casual-agenda-tmenu)
           ("M-j" . org-agenda-clock-goto) ; optional
           ("J" . bookmark-jump))
    :after (org-agenda))

  (use-package ob-async :ensure t :after ob)
  (use-package ob-restclient :ensure t :after ob)

  (use-package boxy-headings :ensure t)

  (use-package org-babel-eval-in-repl :ensure t
    :after ob
    :bind
    (:map org-mode-map
          ("C-c C-<return>" . ober-eval-block-in-repl)))

  (use-package ox-hugo :ensure t :after ox :disabled t
    :config
    (dolist (ext '("zip" "ctf"))
      (push ext org-hugo-external-file-extensions-allowed-for-copying)))

  (use-package org-superstar :ensure t
    :config
    (setq org-superstar-leading-bullet ?\s))

  (use-package org-re-reveal :ensure t :after ox)

  (add-hook 'org-mode-hook
            #'(lambda () (setq line-spacing 0.2) ;; Add more line padding for readability
                ))

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   :map org-mode-map
   ([C-tab] . nil)
   ([C-backtab] . nil)
   ("M-n" . outline-next-visible-heading)
   ("C-c k" . endless/insert-key)
   ("M-p" . outline-previous-visible-heading))
  :config
  (defun compro/right-angled-triangle-with-theta ()
    (interactive)
    (insert "
\\begin{center}
\\begin{tikzpicture}
% Define the triangle vertices
\\coordinate [label=below:$A$] (A) at (0,0);
\\coordinate [label=above:$B$] (B) at (0,2);
\\coordinate [label=below:$C$] (C) at (3,0);

% Draw the triangle
\\draw (A) -- (B) -- (C) -- cycle;

% Label the sides
\\node at ($(A)!0.5!(B)$) [left] {$p$};
\\node at ($(B)!0.5!(C)$) [above] {$h$};
\\node at ($(C)!0.5!(A)$) [below] {$b$};

% Mark the right angle at B
\\draw pic[draw, angle radius=3mm, angle eccentricity=1.5] {right angle = B--A--C};
\\draw pic[draw, angle radius=5mm, \"$\\theta$\", angle eccentricity=1.5] {angle = B--C--A};
\\end{tikzpicture}
\\end{center}
"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; (define-key org-mode-map "\C-ck" #'endless/insert-key)
  (defun endless/insert-key (key)
    "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
    (interactive "kType key sequence: ")
    (let* ((is-org-mode (derived-mode-p 'org-mode))
           (tag (if is-org-mode
                    "@@html:<kbd>%s</kbd>@@"
                  "<kbd>%s</kbd>")))
      (if (null (equal key "\r"))
          (insert
           (format tag (help-key-description key nil)))
        (insert (format tag ""))
        (forward-char (if is-org-mode -8 -6)))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (restclient . t)
     (emacs-lisp . t)))

  (setq org-return-follows-link t
        org-agenda-diary-file "~/.org/diary.org"
        org-src-window-setup 'current-window
        org-startup-with-inline-images t
        org-image-actual-width 400
        org-hierarchical-todo-statistics nil
        org-checkbox-hierarchical-statistics nil
        org-src-preserve-indentation nil
        org-adapt-indentation t)

  (defun my-org-autodone (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'my-org-autodone)
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
    (cl-macrolet
        ((inc-suffixf
           (place)
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
                 (heading (replace-regexp-in-string "[^A-Za-z0-9]" "-" (substring-no-properties (org-get-heading t t t t))))
                 (new-id heading)  ;; (new-id (concat "h-" heading))
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
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp :tangle init.el"))

  (setq org-pretty-entities t
        org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
        org-ellipsis (if is-windows "..." " ")
        org-hide-emphasis-markers t    ;; show actually italicized text instead of /italicized text/
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-default-notes-file "/home/compro/Dropbox/programs/notes/notes.org"
        org-todo-keywords '((sequence "TODO(t)" "inPROGRESS(i)" "|" "DONE(d)" "CANCELED(c)"))

        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "** TODO %?\n  %i\n  %a")
          ("l" "Link" entry (file+headline "~/notes.org" "Links")
           "** %T %^L \n%?"))

        org-todo-keyword-faces
        '(("DONE" . (:inherit org-done :strike-through t))
          ("TODO" . (:inherit org-warning :inverse-video t))
          ("CANCELED" . (:inherit org-verbatim
                                  :box-around-text t
                                  :strike-through t))
          ("inPROGRESS" . (:foreground "orange" :inverse-video t)))))

(with-eval-after-load 'org
  ;; Function redefinition
  (defun org-display-inline-images (&optional include-linked refresh beg end)
    "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
    (interactive "P")
    (when (display-graphic-p)
      (unless refresh
        (org-remove-inline-images)
        (when (fboundp 'clear-image-cache) (clear-image-cache)))
      (let ((end (or end (point-max))))
        (org-with-point-at (or beg (point-min))
          (let* ((case-fold-search t)
                 (file-extension-re (image-file-name-regexp))
                 (link-abbrevs (mapcar #'car
                                       (append org-link-abbrev-alist-local
                                               org-link-abbrev-alist)))
                 ;; Check absolute, relative file names and explicit
                 ;; "file:" links.  Also check link abbreviations since
                 ;; some might expand to "file" links.
                 (file-types-re
                  (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?file:\\)"
                          (if (not link-abbrevs) ""
                            (concat "\\|" (regexp-opt link-abbrevs))))))
            (while (re-search-forward file-types-re end t)
              (let* ((link (org-element-lineage
                            (save-match-data (org-element-context))
                            '(link) t))
                     (linktype (org-element-property :type link))
                     (inner-start (match-beginning 1))
                     (path
                      (cond
                       ;; No link at point; no inline image.
                       ((not link) nil)
                       ;; File link without a description.  Also handle
                       ;; INCLUDE-LINKED here since it should have
                       ;; precedence over the next case.  I.e., if link
                       ;; contains filenames in both the path and the
                       ;; description, prioritize the path only when
                       ;; INCLUDE-LINKED is non-nil.
                       ((or (not (org-element-property :contents-begin link))
                            include-linked)
                        (and (or (equal "file" linktype)
                                 (equal "attachment" linktype))
                             (org-element-property :path link)))
                       ;; Link with a description.  Check if description
                       ;; is a filename.  Even if Org doesn't have syntax
                       ;; for those -- clickable image -- constructs, fake
                       ;; them, as in `org-export-insert-image-links'.
                       ((not inner-start) nil)
                       (t
                        (org-with-point-at inner-start
                          (and (looking-at
                                (if (char-equal ?< (char-after inner-start))
                                    org-link-angle-re
                                  org-link-plain-re))
                               ;; File name must fill the whole
                               ;; description.
                               (= (org-element-property :contents-end link)
                                  (match-end 0))
                               (match-string 2)))))))
                (when (and path (string-match-p file-extension-re path))
                  (let ((file (if (equal "attachment" linktype)
                                  (progn
                                    (require 'org-attach)
                                    (ignore-errors (org-attach-expand path)))
                                (expand-file-name path))))
                    (when (and file (file-exists-p file))
                      (let ((width (org-display-inline-image--width link))
                            (rotation (org-display-inline-image--rotation link))
                            (old (get-char-property-and-overlay
                                  (org-element-property :begin link)
                                  'org-image-overlay)))
                        (if (and (car-safe old) refresh)
                            (image-refresh (overlay-get (cdr old) 'display))
                          (let ((image (org--create-inline-image file width rotation)))
                            (when image
                              (let ((ov (make-overlay
                                         (org-element-property :begin link)
                                         (progn
                                           (goto-char
                                            (org-element-property :end link))
                                           (skip-chars-backward " \t")
                                           (point)))))
                                (overlay-put ov 'display image)
                                (overlay-put ov 'face 'default)
                                (overlay-put ov 'org-image-overlay t)
                                (overlay-put
                                 ov 'modification-hooks
                                 (list 'org-display-inline-remove-overlay))
                                (when (boundp 'image-map)
                                  (overlay-put ov 'keymap image-map))
                                (push ov org-inline-image-overlays))))))))))))))))

  ;; Function redefinition
  (defun org--create-inline-image (file width rotation)
    "Create image located at FILE, or return nil.
 WIDTH is the width of the image.  The image may not be created
 according to the value of `org-display-remote-inline-images'."
    (let* ((remote? (file-remote-p file))
           (file-or-data
            (pcase org-display-remote-inline-images
              ((guard (not remote?)) file)
              (`download (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally file)
                           (buffer-string)))
              (`cache (let ((revert-without-query '(".")))
                        (with-current-buffer (find-file-noselect file)
                          (buffer-string))))
              (`skip nil)
              (other
               (message "Invalid value of `org-display-remote-inline-images': %S"
                        other)
               nil))))
      (when file-or-data
        (create-image file-or-data
                      (and (image-type-available-p 'imagemagick)
                           width
                           'imagemagick)
                      remote?
                      :width width
                      :rotation rotation))))

  ;; New function
  (defun org-display-inline-image--rotation (link)
    "Determine the display rotation of the image LINK, in degrees."
    ;; Apply `org-image-actual-width' specifications.
    (let* ((case-fold-search t)
           (par (org-element-lineage link '(paragraph)))
           (attr-re "^[ \t]*#\\+attr_.*?: +.*?:rotation +\\(\\S-+\\)")
           (par-end (org-element-property :post-affiliated par))
           ;; Try to find an attribute providing a :rot.
           (attr-rot
            (when (and par (org-with-point-at
                               (org-element-property :begin par)
                             (re-search-forward attr-re par-end t)))
              (match-string 1))))
      (when attr-rot (string-to-number attr-rot)))))

(use-package rust-mode :ensure t)

(use-package cargo :ensure t
  :hook (rust-mode . cargo-minor-mode))

;; (use-package company-web :ensure t :after mhtml-mode)

;; (use-package ac-html-csswatcher :ensure t :after mhtml-mode)

(use-package mhtml-mode
  :when (>= emacs-major-version 26)
  :mode ("\\.vue\\'" "\\.html\\'" "\\.jsx")
  :hook (mhtml-mode-hook . sgml-electric-tag-pair-mode)
  :config
  (setq mhtml-tag-relative-indent nil)
  ;; (require 'company)                                   ; load company mode
  ;; (require 'company-web-html)                          ; load company mode html backend
  ;; ;; and/or
  ;; (require 'company-web-jade)                          ; load company mode jade backend
  ;; (require 'company-web-slim)                          ; load company mode slim backend
  ;; (require 'ac-html-csswatcher)
  ;; (company-web-csswatcher-setup)
  ;; (define-key mhtml-mode-map (kbd "C-'") 'company-web-html)
  ;; (add-hook 'mhtml-mode-hook (lambda ()
  ;;                            (set (make-local-variable 'company-backends) '(company-web-html company-files))
  ;;                            (company-mode t)))
  )

(use-package web-mode :ensure t)

(use-package elf-mode :ensure t)

(use-package cmake-mode :ensure t)

(use-package plantuml-mode :ensure t
  :when (locate-file "plantuml.jar" '("~/Downloads"))
  :config
  (setq plantuml-jar-path "~/Downloads/plantuml.jar"))

(use-package typescript-mode :ensure t)

(use-package treemacs :ensure t
  :bind (:map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action))
  :config
  (treemacs-resize-icons 17)
  (setq treemacs-read-string-input 'from-minibuffer))

(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

(defun comint-write-history-on-exit ()
  "Save comint history

References:
1. https://github.com/manzyuk/dotfiles/blob/130f86385f645f0a3a7ee6b31a479c6de2c5ce82/.emacs.d/init.el#L183C1-L188C62
2. https://emacs.stackexchange.com/questions/9720/savehist-the-comint-input-ring"
  (message "In write")
  (when comint-input-ring-file-name
    (unless (file-exists-p (file-name-directory comint-input-ring-file-name))
      (make-directory (file-name-directory comint-input-ring-file-name) t))
    (message (concat "Writing " comint-input-ring-file-name))
    (comint-write-input-ring)))

(add-hook 'kill-buffer-hook 'comint-write-input-ring)
(add-hook 'kill-emacs-hook (lambda () (mapc-buffers 'comint-write-input-ring)))

(use-package python
  :bind (:map
         python-mode-map
         ("TAB" . python-indent-shift-right)
         ("S-TAB" . python-indent-shift-left)
         ("<backtab>" . python-indent-shift-left)
         ("S-<iso-lefttab>" . python-indent-shift-left)

         :map
         python-ts-mode-map
         ("TAB" . python-indent-shift-right)
         ("S-TAB" . python-indent-shift-left)
         ("<backtab>" . python-indent-shift-left)
         ("S-<iso-lefttab>" . python-indent-shift-left))
  :hook (((python-mode python-ts-mode)
          . (lambda ()
              (setq-local fill-column 85
                          forward-sexp-function nil)))
         (inferior-python-mode
          .
          (lambda ()
            (setq-local
             comint-input-ring-file-name
             (concat
              cache-d
              (file-name-as-directory "python-history")
              (s-replace
               "/" "!"
               (or
                (pet-project-root)
                (buffer-file-name)
                (s-replace "*" "+" (buffer-name))))))
            (message (concat "Set " comint-input-ring-file-name))
            (when (file-exists-p comint-input-ring-file-name)
              (message (concat "Reading " comint-input-ring-file-name))
              (comint-read-input-ring)))))
  :config
  (setq python-indent-guess-indent-offset-verbose nil
        python-shell-dedicated 'project))

(use-package flymake-ruff :ensure t
  :hook ((python-mode-hook . flymake-mode)
         (python-mode-hook . flymake-ruff-load)))

(use-package eglot
  :when (>= emacs-major-version 29)
  :config
  (defun compro/python-lsp-setup-for-pyright (&rest r)
    (when-let* ((is-python (seq-contains-p '(python-mode python-ts-mode) major-mode))
                (root (expand-file-name (project-root (project-current))))
                (pyright-exe (executable-find "pyright"))
                (pyrightconfig.json (expand-file-name "pyrightconfig.json" root))
                (config-not-exists (not (f-exists-p pyrightconfig.json)))
                (config-data "")
                (break-apart
                 (lambda (cmd)
                   (when-let ((virtualenv-root-1 (string-split (shell-command-to-string cmd) "\n"))
                              (virtualenv-root-exists (= (length virtualenv-root-1) 2))
                              (virtualenv-root (car virtualenv-root-1))
                              (part1 (file-name-parent-directory virtualenv-root))
                              (part2 (car (last (file-name-split virtualenv-root)))))
                     `(,part1 . ,part2)))))
      (progn
        (if-let* ((poetry-exe (executable-find "poetry"))
                  (parts (funcall break-apart "poetry env info -p")))
            (setq config-data (json-encode (list :venvPath (car parts) :venv (cdr parts))))
          (if-let* ((pipenv-exe (executable-find "pipenv"))
                    (parts (funcall break-apart "pipenv --venv")))
              (setq config-data (json-encode (list :venvPath (car parts) :venv (cdr parts))))
            (message "compro/python-lsp-setup-for-pyright: no pipenv / poetry virtualenv found")))
        (when (not (string= config-data ""))
          (with-temp-file pyrightconfig.json (insert config-data))
          (message (concat "compro/python-lsp-setup-for-pyright: created " pyrightconfig.json))))))

  (advice-add 'eglot :before 'compro/python-lsp-setup-for-pyright))

(use-package pet :ensure t
  :hook ((python-mode . compro/set-python-variables)
         (python-ts-mode . compro/set-python-variables))
  :init
  (defun compro/get-exe (root name)
    (when-let* ((location (concat root "/bin/" name))
                (exists (file-exists-p location)))
      location))
  (defun compro/set-python-variables ()
    (let* ((env-root (or (pet-virtualenv-root) "/usr"))
           (default-directory (or (when env-root (pet-project-root)) default-directory))
           (ipython3 (compro/get-exe env-root "ipython3"))
           (python (or
                    (compro/get-exe env-root "python3")
                    (compro/get-exe env-root "python2")
                    (compro/get-exe env-root "python")))
           (dj-root (locate-dominating-file default-directory "manage.py"))
           (manage.py (when dj-root (expand-file-name "manage.py" dj-root)))
           (default-directory (if dj-root (pet-project-root) default-directory))
           (dj-shell-plus (when dj-root (= 0 (call-process python nil nil nil manage.py "help" "shell_plus")))))
      (when manage.py
        (setq-local
         python-shell-interpreter-args
         (concat
          " "
          manage.py
          " "
          (cond
           ((and ipython3 dj-shell-plus)
            "shell_plus --ipython -- -i --simple-prompt --classic")
           (ipython3
            "shell --command \"from IPython import start_ipython; start_ipython(argv=[\'-i\', \'--simple-prompt\',\'--classic\'])\"")
           (dj-shell-plus
            "shell_plus --plain -- -i")
           (t
            "shell -i python"))))
        (setq ipython3 nil))
      (cond
       (ipython3 (setq-local
                  py-use-local-default t
                  py-shell-local-path ipython3
                  python-shell-interpreter ipython3
                  python-shell-interpreter-args "-i --simple-prompt --classic"))
       (python (setq-local
                py-use-local-default t
                py-shell-local-path python
                python-shell-interpreter python
                python-shell-interpreter-args "-i -m asyncio")))

      (setq-local python-shell-virtualenv-root env-root
                  lsp-pyright-venv-path env-root
                  lsp-pyright-python-executable-cmd python
                  dap-python-executable python
                  python-pytest-executable (pet-executable-find "pytest")
                  exec-path (append `(,(concat env-root "/bin")) exec-path)))

    ;; (when-let ((black-executable (pet-executable-find "black")))
    ;;   (setq-local python-black-command black-executable)
    ;;   (python-black-on-save-mode 1))

    ;; (when-let ((isort-executable (pet-executable-find "isort")))
    ;;   (setq-local python-isort-command isort-executable)
    ;;   (python-isort-on-save-mode 1))
    ))

(use-package vterm :ensure t :when is-linux
  :init
  (defun vterm-directory-sync ()
    "Synchronize current working directory."
    (interactive)
    (when vterm--process
      (let* ((pid (process-id vterm--process))
             (dir (file-truename (format "/proc/%d/cwd/" pid))))
        (setq default-directory dir))))
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-buffer-name-string "*vterm-%s*"
        vterm-always-compile-module t))

(use-package treesit-auto :ensure t
  :when (>= emacs-major-version 29)
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package eshell-syntax-highlighting :ensure t :after esh-mode
  :config (eshell-syntax-highlighting-global-mode +1))

(use-package embrace :ensure t
  :bind
  ("C-," . embrace-commander))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(use-package ligature :ensure nil
  :unless (or (< emacs-major-version 27) is-windows)
  :init
  (require 'quelpa)
  (when (not (quelpa--package-installed-p 'ligature))
    (quelpa
     '(ligature
       :fetcher url
       :url "https://raw.githubusercontent.com/mickeynp/ligature.el/master/ligature.el")))
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures
   'eww-mode
   '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package diff-hl :ensure t

  :hook (;; Sync with git (specifically magit) operations
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)
         (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)

         ;; Enable in local dired mode
         (dired-mode-hook . diff-hl-dired-mode-unless-remote))

  :config
  ;; Disable diff-hl on remote files to prevent slowness
  (setq diff-hl-disable-on-remote t)

  (global-diff-hl-mode t)
  (diff-hl-margin-mode t)
  (diff-hl-flydiff-mode t))

(use-package whole-line-or-region :ensure t
  :config (whole-line-or-region-global-mode +1))

(use-package cascading-dir-locals :ensure t
  :config
  (cascading-dir-locals-mode 1))

(use-package just-mode :ensure t)

(use-package numpydoc :ensure t
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

(use-package chembalance :ensure t)

(use-package eping :ensure t)

(use-package go-mode :ensure t)

(use-package filetree :ensure t)

(use-package flymake-flycheck :ensure t)

(use-package bash-completion :ensure t
  :config
  (bash-completion-setup))

(use-package apheleia :ensure t
  :config
  ;; (setf (alist-get 'isort apheleia-formatters)
  ;;     '("usort" "format" "-"))
  (defun compro/black ()
    (if-let* ((root (pet-virtualenv-root))
              (executable (concat root "/bin/black"))
              (exists (file-exists-p executable)))
        `(,executable "-")
      "cat"))
  (defun compro/djhtml ()
         (if-let* ((root (pet-virtualenv-root))
                   (executable (concat root "/bin/djhtml"))
                   (exists (file-exists-p executable)))
             `(,executable "-")
           "cat"))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(black-custom)
        (alist-get 'black-custom apheleia-formatters) '((compro/black))
        (alist-get 'djhtml apheleia-formatters) '((compro/djhtml))
        (alist-get 'rustfmt apheleia-formatters) '("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2021"))
  (apheleia-global-mode +1))

(use-package narrow-reindent :ensure t
  :hook (find-file-hook . narrow-reindent-mode))

(use-package daemons :ensure t)

(use-package all-the-icons-completion :ensure t
  :when (display-graphic-p)
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode 1))

(use-package flimenu :ensure t
  :config
  (flimenu-global-mode 1))

(use-package coterm :ensure t :disabled t
  :config
  (coterm-mode 1))

(use-package git-modes :ensure t)

(use-package async-backup :ensure t
  :hook (after-save-hook . async-backup))

(use-package subed :ensure t
  ;; :init
  ;; ;; Disable automatic movement of point by default
  ;; (add-hook 'subed-mode-hook 'subed-disable-sync-point-to-player)
  ;; ;; Remember cursor position between sessions
  ;; (add-hook 'subed-mode-hook 'save-place-local-mode)
  ;; ;; Break lines automatically while typing
  ;; (add-hook 'subed-mode-hook 'turn-on-auto-fill)
  ;; ;; Break lines at 40 characters
  ;; (add-hook 'subed-mode-hook (lambda () (setq-local fill-column 40)))
  )

(use-package buffer-move :ensure t)

(use-package redacted :ensure t
  :init
  (add-hook 'redacted-mode-hook (lambda () (read-only-mode (if redacted-mode 1 -1)))))

(use-package cycle-at-point :ensure t
  :bind (("M-p" . cycle-at-point)
         ("M-n" . (lambda ()
                    (interactive)
                    (let ((current-prefix-arg '(-1)))
                      (call-interactively 'cycle-at-point))))))

(use-package comint-mime :ensure t :when (display-graphic-p)
  :hook
  ((shell-mode-hook . comint-mime-setup)
   (inferior-python-mode-hook . comint-mime-setup)))

(use-package flymake-collection :ensure t
  :config
  (flymake-collection-hook-setup)
  (push
   '(python-mode
     flymake-collection-mypy                      ; Always added to diagnostic functions.
     (flymake-collection-pycodestyle :disabled t) ; Never added.
     (flymake-collection-pylint                   ; Added when predicate is true.
      :predicate (lambda ()
                   (executable-find "pylint"))))
   flymake-collection-config))

(use-package ruby-electric :ensure t
  :hook (ruby-mode-hook . ruby-electric-mode))

(use-package rbenv :ensure t
  :config
  (global-rbenv-mode)
  (rbenv-use-corresponding))

(use-package inf-ruby :ensure t
  :bind (:map inf-ruby-minor-mode-map
              ("C-c C-c" . ruby-send-buffer-and-go)))

(use-package ruby-test-mode :ensure t
  :hook (ruby-mode-hook . ruby-test-mode))

(use-package rinari :ensure t
  :config
  (global-rinari-mode))

(use-package yari :ensure t
  :hook (ruby-mode-hook . ri-bind-key)
  :init
  (defun ri-bind-key ()
    (local-set-key [f1] 'yari)))

(use-package fancy-compilation :ensure t :after compile
  :config
  (fancy-compilation-mode))

(use-package repeat-help :ensure t
  :hook (repeat-mode-hook . repeat-help-mode))

(use-package yaml-pro :ensure t
  :hook (yaml-mode-hook . yaml-pro-mode))

(use-package clean-kill-ring :ensure t
  :config
  (setq clean-kill-ring-prevent-duplicates t)
  (clean-kill-ring-mode 1))

(use-package eldoc-box :ensure t
  :hook (prog-mode-hook . eldoc-box-hover-at-point-mode))

(use-package editorconfig :ensure t
  :config
  (add-to-list 'editorconfig-indentation-alist
               '(mhtml-mode js-indent-level
                            css-indent-offset
                            sgml-basic-offset))
  (editorconfig-mode 1))

(use-package yaml-mode :ensure t)

(use-package indent-bars :ensure t)

(use-package otpp
  :ensure t
  :after project
  :init
  ;; If you like to define some aliases for better user experience
  (defalias 'one-tab-per-project-mode 'otpp-mode)
  (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
  ;; Enable `otpp-mode' globally
  (otpp-mode 1)
  ;; If you want to advice the commands in `otpp-override-commands'
  ;; to be run in the current's tab (so, current project's) root directory
  (otpp-override-mode 1))

(use-package casual-editkit
  :ensure t
  :bind (("C-o" . casual-editkit-main-tmenu)))

(use-package casual-symbol-overlay
  :ensure nil
  :bind (:map
         symbol-overlay-map
         ("C-o" . casual-symbol-overlay-tmenu)))

(use-package show-font :ensure t)

(use-package stillness-mode :ensure t :demand t
  :config
  (stillness-mode 1))

(use-package treesit-fold :ensure t)

(use-package breadcrumb :ensure t
  :init
  (breadcrumb-mode 1))

(require 'ultra-scroll)
(ultra-scroll-mode 1)

(unless (package-installed-p 'pg)
   (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
(unless (package-installed-p 'pgmacs)
   (package-vc-install "https://github.com/emarsden/pgmacs" nil nil 'pgmacs))

(require 'pgmacs)

(quelpa '(p-search :repo "zkry/p-search" :fetcher github))
(require 'p-search)

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(when (> emacs-major-version 27)
  (set-face-attribute 'tab-bar-tab nil :box nil))
(when (< emacs-major-version 31)
  (minions-mode 1))
(setq debug-on-error  nil
      init-file-debug nil)
(compro/redownload-empty-pkgs)

;; Remove text property from text in kill-ring
(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)
