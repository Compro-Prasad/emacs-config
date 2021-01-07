;;; init.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020  Abhishek(Compro) Prasad

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

(setq cache-d (locate-user-emacs-file (concat emacs-d ".cache/"))
      package-user-dir (concat cache-d "elpa/"))

(require 'seq)
(setq is-windows
      (seq-filter
       (lambda (x) (string= system-type x))
       '("ms-dos" "windows-nt" "cygwin")))
(setq is-unix
      (seq-filter
       (lambda (x) (string= system-type x))
       '("gnu" "gnu/linux" "gnu/kfreebsd" "darwin" "cygwin")))
(setq is-gnu
      (seq-filter
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
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
  (add-to-list 'package-archives (cons "tree-sitter" (concat proto "://elpa.ubolonton.org/packages/"))))
(package-initialize)

(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(leaf leaf)

(leaf f :leaf-defer nil :ensure t :require t)
(leaf s :leaf-defer nil :ensure t :require t)

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

(defun compro/comint/last-output-beg ()
  (save-excursion
    (comint-goto-process-mark)
    (while (not (or (eq (get-char-property (point) 'field) 'boundary)
                    (= (point) (point-min))))
      (goto-char (previous-char-property-change (point) (point-min))))
    (if (= (point) (point-min))
        (point)
      (1+ (point)))))

(defun compro/comint/last-output-end ()
  (save-excursion
    (comint-goto-process-mark)
    (while (not (or (eq (get-char-property (point) 'font-lock-face)
                        'comint-highlight-prompt)
                    (= (point) (point-min))))
      (goto-char (previous-char-property-change (point) (point-min))))
    (let ((overlay (car (overlays-at (point)))))
      (when (and overlay (eq (overlay-get overlay 'font-lock-face)
                             'comint-highlight-prompt))
        (goto-char (overlay-start overlay))))
    (1- (point))))

(defun compro/comint/clear-last-output ()
  (interactive)
  (let ((start (compro/comint/last-output-beg))
        (end (compro/comint/last-output-end)))
    (let ((inhibit-read-only t))
      (delete-region start end)
      (save-excursion
        (goto-char start)
        (insert (propertize "output cleared"
                            'font-lock-face 'font-lock-comment-face))))))

(defun compro/comint/preoutput-read-only (text)
  (propertize text 'read-only t))

(defun compro/shell-turn-echo-off ()
  (setq comint-process-echoes t))
(add-hook 'shell-mode-hook 'compro/shell-turn-echo-off)

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

(setq compro/laptop-p (equal system-name "c-p-dell-manjaro"))

(leaf general :leaf-defer nil :ensure t :require t)

(make-directory "~/.ssh/sockets" t)

(defvar disable-tramp-backups '(all))

(eval-after-load "tramp"
  '(progn
     ;; Modified from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
     (setq backup-enable-predicate
           (lambda (name)
             (and (normal-backup-enable-predicate name)
              ;; Disable all tramp backups
              (and disable-tramp-backups
                   (member 'all disable-tramp-backups)
                   (not (file-remote-p name 'method)))
              (not ;; disable backup for tramp with the listed methods
               (let ((method (file-remote-p name 'method)))
                 (when (stringp method)
                   (member method disable-tramp-backups)))))))

     (defun tramp-set-auto-save--check (original)
       (if (funcall backup-enable-predicate (buffer-file-name))
           (funcall original)
         (auto-save-mode -1)))

     (advice-add 'tramp-set-auto-save :around #'tramp-set-auto-save--check)

     ;; Use my ~/.ssh/config control master settings according to https://puppet.com/blog/speed-up-ssh-by-reusing-connections
     (setq tramp-ssh-controlmaster-options ""
           remote-file-name-inhibit-cache 30)))

(leaf tab-bar :leaf-defer nil :require t
  :when (> emacs-major-version 27)
  :bind (("C-t" . tab-bar-new-tab-event)
         ([C-f4] . tab-bar-close-tab)
         ("C-S-t" . tab-bar-undo-close-tab)
         ([C-tab] . tab-next)
         ([C-backtab] . tab-previous)
         ([C-S-tab] . tab-previous)
         ([C-iso-lefttab] . tab-previous))
  :init
  (tab-bar-mode)

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
       (format "[%s] %s"
               (propertize (key-description `(,key)) 'face 'bold)
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
  (when (fboundp 'doom-color)
    (let ((bg (doom-color 'bg))
          (fg (doom-color 'fg))
          (base1 (doom-color 'base1))
          (box-width 7))
      (set-face-attribute 'tab-bar nil :background base1 :foreground fg)
      (set-face-attribute 'tab-bar-tab nil :background bg :box (list :line-width box-width :color bg) :weight 'bold)
      (set-face-attribute 'tab-bar-tab-inactive nil :background base1 :box (list :line-width box-width :color base1)))))

(leaf diary-lib
  :config
  (setq diary-file "~/diary"))

(leaf narrow-reindent :ensure t :leaf-defer nil :require t
  :hook (find-file-hook . narrow-reindent-mode))

(leaf dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :bind ((dired-mode-map
          ("C-c C-c" . dired-collapse-mode)
          ("C-c C-d C-u" . dired-du-mode)
          ("." . dired-hide-dotfiles-mode)
          ("<tab>" . dired-subtree-toggle)
          ("q"      . kill-current-buffer)
          ("RET"    . compro/dired-open-dir)
          ("^"      . compro/dired-up-dir)
          ("DEL"    . compro/dired-up-dir)
          ("<left>" . compro/dired-up-dir)))
  :preface
  (leaf dired-x
    :bind (("C-x <C-j>" . dired-jump)
           ("C-x C-j" . dired-jump)))
  (leaf dired-collapse :ensure t)
  (leaf dired-du :ensure t :after dired)
  (leaf dired-dups :ensure t :after dired)
  (leaf dired-filetype-face :ensure t :after dired)
  (leaf dired-hide-dotfiles :ensure t
    :after dired
    :hook (dired-mode-hook . dired-hide-dotfiles-mode))
  (leaf dired-subtree :ensure t :after dired)
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
 ;;;   Initial major mode for *scratch* buffer
 initial-major-mode 'fundamental-mode

 ;;;   Node.js path from nvm
 exec-path (append exec-path '("/home/compro/.nvm/versions/node/v12.13.0/bin/"))

 ;;;   User details
 user-mail-address "comproprasad@gmail.com"
 user-full-name "Compro Prasad"

 ;;;   Only use ~/.authinfo.gpg
 auth-sources (list (ft "~/.authinfo.gpg"))

 ;;;   Security settings
 gnutls-verify-error t

 ;;;   Customizations go to this file
 custom-file (expand-file-name "custom.el" cache-d)

 ;;;   Follow symlinks to the actual file
 find-file-visit-truename t
 vc-follow-symlinks t

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
 jit-lock-stealth-time 1

 ;;;   Sentences are separated by single space after dot(.)
 sentence-end-double-space nil

 ;;;   Don't compact font cache during GC to optimize redisplay
 inhibit-compacting-font-caches t

 ;;;   GC triggers per 90 MB increase in memory
 gc-cons-threshold 94371840

 ;;;   Prevent recursion limits
 max-lisp-eval-depth 48000
 max-specpdl-size 10000

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
 erc-server    "107.182.226.199"  ;;; IP for "irc.freenode.net"
 erc-nick      "compro"

 ;;;   Dired
 dired-dwim-target t
 dired-listing-switches "-lAh --group-directories-first"

 ;;;   Ediff
 ediff-window-setup-function 'ediff-setup-windows-plain ;; Single frame ediff session

 ;;;   Ido mode
 ido-enable-flex-matching t
 ido-save-directory-list-file (concat cache-d "ido.last")
 )

(fset 'yes-or-no-p 'y-or-n-p)

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

(menu-bar-mode 0)
(menu-bar-no-scroll-bar)
(blink-cursor-mode 0)
(tool-bar-mode 0)

(column-number-mode 1)
(display-time-mode 1)

(delete-selection-mode 1)

(when (not window-system)
  (xterm-mouse-mode 1))  ; Enable mouse in terminal

(when (display-graphic-p)
  (general-define-key
   :keymaps 'input-decode-map
   [?\C-m] [C-m]
   [?\C-i] [C-i]
   ;; [?\C-j] [C-j]
   [?\C-\[] (kbd "<C-[>")))

(general-define-key
 "C-z"             'undo
 "C-x C-o"         'ff-find-other-file
 [C-m]             'delete-other-windows
 "<C-S-mouse-1>"   'imenu
 "C-c r"           'imenu
 "M-/"             'hippie-expand
 [mouse-3]         menu-bar-edit-menu
 "M-^"             'compile)

(global-auto-revert-mode t)

(show-paren-mode t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(add-hook 'prog-mode-hook 'which-function-mode)

(add-hook 'prog-mode-hook 'electric-pair-mode)

(when (>= emacs-major-version 27)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

(set-default-font '("Fira Code" :size 12 :weight normal :width normal))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize the compilation buffer with ANSI escape sequences."
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun compro/rename-file-buffer ()
  "Rename current buffer and the file it is linked to."
  (interactive)
  (let ((filename (basename (buffer-file-name))))
    (if (and filename (file-exists-p filename))
        (let* ((new-name (read-string
                          (concat "Rename '" filename "' to: ")
                          filename)))
          (rename-file filename new-name 1)
          (set-visited-file-name new-name t t))
      (message "This buffer is not linked to a file"))))
(global-set-key (kbd "C-c f r") 'compro/rename-file-buffer)

(leaf simple
  :bind (("C-a" . compro/beginning-of-line)
         ("C-o" . compro/open-line-below)
         ("C-S-p" . list-processes)
         ("C-S-o" . compro/open-line-above))
  :config
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
          (back-to-indentation)))))
  (defun compro/open-line-below ()
    (interactive)
    (end-of-line)
    (newline-and-indent))
  (defun compro/open-line-above ()
    (interactive)
    (back-to-indentation)
    (newline-and-indent)
    (previous-line 1)
    (indent-according-to-mode)))

(with-eval-after-load 'comint
  (general-define-key
   :kemaps 'comint-mode-map
   "<remap> <kill-word>" 'compro/comint/kill-word
   "C-S-l" 'compro/comint/clear-last-output))

(add-hook 'comint-preoutput-filter-functions
          'compro/comint/preoutput-read-only)

(setq history-length t
      history-delete-duplicates t
      savehist-file (concat cache-d "savehist")
      save-place-file (concat cache-d "saveplace")
      savehist-additional-variables '(kill-ring
                                      extended-command-history
                                      global-mark-ring
                                      mark-ring
                                      regexp-search-ring
                                      search-ring))
(save-place-mode 1)
(savehist-mode 1)

(require 'recentf)
(setq recentf-max-saved-items 512
      recentf-save-file (concat cache-d "recentf"))
(add-to-list 'recentf-exclude
             (concat (regexp-quote (ft (format cache-d))) ".*"))
(recentf-mode 1)

(leaf xwidget
  :when (fboundp 'xwidget-webkit-browse-url)
  :bind
  (xwidget-webkit-mode-map
   ("<mouse-4>" . xwidget-webkit-scroll-down)
   ("<mouse-5>" . xwidget-webkit-scroll-up)
   ("<up>" . xwidget-webkit-scroll-down)
   ("<down>" . xwidget-webkit-scroll-up)
   ("M-w" . xwidget-webkit-copy-selection-as-kill)
   ("C-c" . xwidget-webkit-copy-selection-as-kill))
  :preface
  (defun compro/xwidget-webkit/adjust-size ()
    (when (equal major-mode 'xwidget-webkit-mode)
      (xwidget-webkit-adjust-size-dispatch)))
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
    (xwidget-webkit-browse-url url t)))

(add-hook 'package-menu-mode-hook 'hl-line-mode)

(leaf winner :require t :leaf-defer nil
  :config (winner-mode 1))

(defun compro/set-show-whitespace-mode ()
  "Show white space in current buffer"
  (setq show-trailing-whitespace t))
;; Show whitespaces only in buffers pointing to specific files
(add-hook 'find-file-hook 'compro/set-show-whitespace-mode)
;; Remove the trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000)
  (garbage-collect))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

(c-add-style "mylinux"
             '("linux"
               (tab-width . 4)
               (c-basic-offset . 4)
               (indent-tabs-mode . t)
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

(leaf restclient :ensure t)

(leaf hydra :ensure t)

(leaf hungry-delete :leaf-defer nil :ensure t :require t
  :init (global-hungry-delete-mode t))

(leaf minions :ensure t
  :bind ([S-down-mouse-3] . minions-minor-modes-menu))

(leaf transient :ensure t
  :init
  (setq transient-history-file (locate-user-emacs-file
                                (concat cache-d "transient/history.el"))
        transient-values-file (locate-user-emacs-file
                               (concat cache-d "transient/values.el"))
        transient-levels-file (locate-user-emacs-file
                               (concat cache-d "transient/levels.el"))))

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
  (leaf forge :disabled is-windows :after magit :ensure t :require t)
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

(leaf git-messenger :ensure t
  :bind (("C-x v p" . git-messenger:popup-message)))

(leaf expand-region :ensure t
  :commands (er/expand-region
             er/mark-paragraph
             er/mark-inside-pairs
             er/mark-outside-pairs
             er/mark-inside-quotes
             er/mark-outside-quotes
             er/contract-region)
  :bind (("C-=" . hydra-er/er/expand-region)
         ("C--" . hydra-er/er/expand-region)
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

(leaf projectile :leaf-defer nil :ensure t :require t
  :disabled (> emacs-major-version 27)  ;; Use project.el for > 27
  :bind (("C-x p" . projectile-command-map))
  :config
  (setq
   projectile-cache-file (concat cache-d "projectile")
   projectile-known-projects-file (concat cache-d "projectile-bookmarks.eld")
   projectile-completion-system 'default)
  (projectile-mode 1))

(leaf ag :ensure t :when (executable-find "ag"))

(leaf switch-window :ensure t
  :bind ("C-x o" . switch-window))

(leaf which-key :ensure t
  :init
  (setq which-key-idle-delay (if is-windows 0.212 1.0))
  (which-key-mode))

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

(leaf undo-tree
  :ensure t
  :leaf-defer nil
  :require t
  :bind
  ((:undo-tree-map
    ("C-_" . nil)
    ("C-/" . nil)
    ("C-?" . nil))
   (:global-map
    ("C-_" . nil)))
  :config
  (setq undo-tree-enable-undo-in-region t)
  (global-undo-tree-mode t))

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

(leaf spacemacs-theme  ; Load only in GUI
  :disabled (not window-system)
  :ensure t
  :config (load-theme 'spacemacs-dark t))

(leaf modus-themes  ; Load only in terminal
  :disabled window-system
  :ensure t
  :require t
  :leaf-defer nil
  :config (load-theme 'modus-operandi t))

(leaf page-break-lines :ensure t
  :init
  (global-page-break-lines-mode t))

(leaf pipenv :ensure t
  :bind
  (("<f9> p v a" . pipenv-activate)
   ("<f9> p v d" . pipenv-deactivate)
   ("<f9> p v g" . pipenv-graph)
   ("<f9> p v e" . pipenv-envs)))

(leaf company :ensure t
  :hook (after-init-hook . global-company-mode)
  :config
  (setq company-show-numbers 'left
        company-idle-delay 0.165
        company-minimum-prefix-length 1))

(leaf lsp-mode :ensure t
  :hook (c-mode-common-hook . compro/init-lsp)
  :preface
  ;; (leaf lsp-pyright :ensure t)
  (add-hook 'python-mode-hook
            (lambda ()
              (require 'lsp)
              (pipenv-activate)
              (sleep-for 1)
              (lsp)))
  :init
  (setq lsp-keymap-prefix "<f8>"
        lsp-session-file (locate-user-emacs-file
                          (ft (concat cache-d ".lsp-session-v1")))
        lsp-prefer-capf t
        lsp-idle-delay 0.7)
  (defun compro/init-lsp ()
    "Start lsp server only when it is a valid project where lsp
is useful."
    (when (and (fboundp 'projectile-project-p) (projectile-project-p))
      (lsp)))
  )

(leaf floobits :ensure t)

(leaf selectrum :ensure t
  :bind (("M-y" . compro/yank-pop)
         ("C-x C-r" . recentf-open-files+)
         ("C-x b" . selectrum-switch-buffer+))
  :hook (after-init-hook . selectrum-mode)
  :config
  (leaf selectrum-prescient :ensure t :after selectrum
    :config
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1))
  ;; Wiki - Minibuffer default add function
  (autoload 'ffap-guesser "ffap")
  (setq minibuffer-default-add-function
        (defun minibuffer-default-add-function+ ()
          (with-selected-window (minibuffer-selected-window)
            (delete-dups
             (delq nil
                   (list (thing-at-point 'symbol)
                         (thing-at-point 'list)
                         (ffap-guesser)
                         (thing-at-point-url-at-point)))))))

  (defun compro/yank-pop (&optional arg)
    "Paste a previously killed string.
With just \\[universal-argument] as ARG, put point at beginning,
and mark at end.  Otherwise, put point at the end, and mark at
the beginning without activating it.

This is like `yank-pop'.  The differences are:

- This let you manually choose a candidate to paste.

- This doesn't delete the text just pasted if the previous
  command is `yank'."
    (interactive "P")
    (let* ((selectrum-should-sort-p nil)
           (text nil))
      (setq text
            (completing-read "Yank: "
                             (cl-remove-duplicates
                              kill-ring :test #'equal :from-end t)
                             nil 'require-match))
      (unless (eq last-command 'yank)
        (push-mark))
      (setq last-command 'yank)
      (setq yank-window-start (window-start))
      (when (and delete-selection-mode (use-region-p))
        (delete-region (region-beginning) (region-end)))
      (insert-for-yank text)
      (if (consp arg)
          (goto-char (prog1 (mark t)
                       (set-marker (mark-marker) (point) (current-buffer)))))))

  (defun recentf-open-files+ ()
    "Use `completing-read' to open a recent file."
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file (completing-read "Find recent file: " files nil t))))

  (defun selectrum-switch-buffer+ ()
    (interactive)
    (let* ((selectrum-should-sort-p nil)
           (candidates
            (let* ((cb (window-buffer
                        (minibuffer-selected-window)))
                   (bf (or (buffer-file-name cb) "")))
              (lambda (input)
                (let* ((buffers (mapcar #'buffer-name
                                        (cl-delete-if
                                         (lambda (buf)
                                           (eq buf cb))
                                         (buffer-list))))
                       (files (cl-delete-if (lambda (f) (string= f bf))
                                            (copy-sequence recentf-list)))
                       (candidates ()))
                  (cond ((string-prefix-p " " input)
                         (setq input (substring input 1))
                         (setq candidates
                               (cl-delete-if-not
                                (lambda (name)
                                  (string-prefix-p " " name))
                                buffers)))
                        ((string-prefix-p "b " input)
                         (setq input (substring input 2))
                         (setq candidates
                               (cl-delete-if
                                (lambda (name)
                                  (string-prefix-p " " name))
                                buffers)))
                        ((string-prefix-p "f " input)
                         (setq input (substring input 2))
                         (setq candidates files))
                        (t
                         (setq candidates
                               (append
                                (cl-delete-if
                                 (lambda (name)
                                   (string-prefix-p " " name))
                                 buffers)
                                files))))
                  `((candidates . ,candidates)
                    (input . ,input))))))
           (cand (selectrum-read "Switch to: " candidates)))
      (cond ((member cand recentf-list)
             (find-file cand))
            (t
             (switch-to-buffer cand)))))

  (cl-defmacro selectrum-make-action ((&rest args) &body body)
    (declare (indent 1))
    `(lambda ()
       (interactive)
       (put 'quit 'error-message "")
       (run-at-time nil nil
                    (lambda (,@args)
                      (put 'quit 'error-message "Quit")
                      (with-demoted-errors "Error: %S"
                        ,@body))
                    ,@(seq-take
                       `((if selectrum--refined-candidates (nth selectrum--current-candidate-index selectrum--refined-candidates))
                         selectrum--refined-candidates
                         (selectrum-get-current-input)
                         selectrum--current-candidate-index)
                       (length args)))
       (abort-recursive-edit)))

  (defvar selectrum-search-rg-history nil)

  (defun im/search-rg+ ()
    "Search like 'counsel-rg'.

Default, search for current directory, if the input begin with 'p ' then
will search current project, if begin with 'o ' then will search org-directory.

'C-c C-o' to pop the rg.el's Occur view, make sure package `rg' is installed."
    (interactive)
    (unless (executable-find "rg")
      (user-error "ripgrep must be installed."))
    (let* (type
           input
           (dir default-directory)
           (word (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (let* ((sym (symbol-at-point)) (symn (symbol-name sym)))
                     (if (and sym (> 50 (length symn) 3)) symn nil))))
           (command (if (memq system-type '(ms-dos windows-nt))
                        "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R> ."
                      "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R>"))
           (cands (lambda (in)
                    (let ((msg)
                          (prop (lambda (cs)
                                  (mapcar (lambda (c)
                                            (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):" c)
                                              (add-face-text-property (match-beginning 1) (match-end 1) 'compilation-info nil c)
                                              (add-face-text-property (match-beginning 2) (match-end 2) '(:underline t :inherit compilation-line-number) nil c))
                                            c)
                                          cs))))
                      (cond
                       ;; search current project
                       ((string-prefix-p "p " in)
                        (cond ((not (project-current))
                               (setq msg "This is not in a project."))
                              ((< (length in) 5)
                               (setq msg "Search in current project, input should more than 3."))
                              (t
                               (setq type 'project)
                               (setq dir (cdr (project-current)))
                               (setq in (cl-subseq in 2)))))
                       ;; search org-directory
                       ((string-prefix-p "o " in)
                        (cond ((not (file-exists-p org-directory))
                               (setq msg "Org Directory not exist?"))
                              ((< (length in) 5)
                               (setq msg "Search in org-directory, input should more than 3."))
                              (t
                               (setq type 'org)
                               (setq dir org-directory)
                               (setq in (cl-subseq in 2)))))
                       ;; search current directory
                       (t (if (< (length in) 3)
                              (setq msg "Input should more than 3."))
                          (setq type nil)
                          (setq dir default-directory)))
                      ;; take space in INPUT as .*?
                      ;; take m-space as [[:blank:]]
                      (setq input
                            (replace-regexp-in-string
                             " +" "[[:blank:]]"
                             (replace-regexp-in-string
                              "\\([^ ]\\) \\([^ ]\\)" "\\1.+?\\2"
                              (string-trim in))))
                      (if msg
                          (prog1 nil
                            (setq-local selectrum-refine-candidates-function
                                        (lambda (_ __) (list msg))))
                        (kill-local-variable 'selectrum-refine-candidates-function)
                        (let* ((default-directory dir)
                               (cs (split-string
                                    (shell-command-to-string (grep-expand-template command input)) "\n")))
                          `((candidates . ,(funcall prop cs))
                            (input . ,input)))))))
           (cand (let ((selectrum-should-sort-p nil)
                       (selectrum-minibuffer-bindings
                        (append
                         selectrum-minibuffer-bindings
                         `(("C-c C-o" . ,(selectrum-make-action (c)
                                           ;; use rg.el to show the results in Occur buffer
                                           (require 'rg)
                                           (require 'compile)
                                           ;; jump to current candidate in the *rg* buffer.
                                           ;; rg implemented with `compile', so I make it work like below.
                                           ;; let-bound method not working, unkown reason.
                                           (let ((old-compilation-finish-functions compilation-finish-functions))
                                             (setq compilation-finish-functions
                                                   (list
                                                    (lambda (_a _b)
                                                      (unwind-protect
                                                          (progn
                                                            (pop-to-buffer (current-buffer))
                                                            (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" c)
                                                              (let ((file-name (match-string-no-properties 1 c))
                                                                    (line-number (match-string-no-properties 2 c)))
                                                                (if rg-group-result
                                                                    (progn
                                                                      (re-search-forward (format "^File: %s" file-name) nil t)
                                                                      (re-search-forward (format "^ *%s" line-number) nil t)
                                                                      (re-search-forward input (point-at-eol) t))
                                                                  (re-search-forward (format "%s:%s:" file-name line-number) nil t)
                                                                  (re-search-forward input (point-at-eol) t)))))
                                                        (setq compilation-finish-functions old-compilation-finish-functions)))))
                                             ;; dispatch to rg.el search.
                                             (cond ((eq type 'project) (rg-project input "*"))
                                                   (t                  (rg input "*" dir))))))))))
                   (selectrum-read "rg: " cands
                                   :initial-input word
                                   :may-modify-candidates t
                                   :history 'selectrum-search-rg-history
                                   :require-match t))))
      (if (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
          (let ((file-name (match-string-no-properties 1 cand))
                (line-number (match-string-no-properties 2 cand)))
            (xref-push-marker-stack) ; use M-, to go back!
            (find-file (expand-file-name file-name dir))
            (goto-char (point-min))
            (forward-line (1- (string-to-number line-number)))
            (re-search-forward input (point-at-eol) t)
            (recenter))
        (message "Bad candidate?")))))

(leaf ctrlf :ensure t :leaf-defer nil :require t
  :config (ctrlf-mode 1))

(leaf yasnippet :ensure t
  :bind ("C-/" . yas-expand)
  :preface
  (leaf yasnippet-snippets :ensure t :after yasnippet)
  :config
  (yas-global-mode 1))

(leaf beginend :ensure t :leaf-defer nil :require t
  :config (beginend-global-mode))

(leaf move-text :ensure t :leaf-defer nil :require t
  :bind
  (("C-_" . move-text-up)
   ("C--" . move-text-down)))

(leaf default-text-scale :ensure t
  :config (default-text-scale-mode 1))

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

(leaf iedit :ensure t
  :bind ("C-c i" . iedit-mode))

(leaf wgrep :ensure t :after grep :require t)

(leaf shackle :ensure t :require t
  :config
  (setq shackle-default-rule '(:select t))
  (setq shackle-rules
        '((help-mode :size 0.33 :select t :align bottom)))
  (shackle-mode 1))

(leaf clang-format+ :ensure t
  :init
  (setq clang-format+-context 'buffer))

(leaf telega :ensure t :when is-linux)

(leaf org :ensure org-plus-contrib
  :preface
  (leaf ob-async :ensure t :require t :after ob)
  (leaf org-babel-eval-in-repl :ensure t
    :after ob
    :bind
    (org-mode-map
     ("C-c C-<return>" . ober-eval-block-in-repl)))
  (leaf org-plus-contrib :ensure t)
  (leaf ox-hugo :require t :ensure t :after ox :disabled t
    :config
    (dolist (ext '("zip" "ctf"))
      (push ext org-hugo-external-file-extensions-allowed-for-copying)))
  (leaf org-bullets :ensure t :require t :after org
    :config (org-bullets-mode 1))
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
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (emacs-lisp . t)))
  (setq org-return-follows-link t
        org-agenda-diary-file "~/.org/diary.org"
        org-src-window-setup 'current-window
        org-startup-with-inline-images t
        org-image-actual-width 400
        org-hierarchical-todo-statistics nil
        org-checkbox-hierarchical-statistics nil
        org-src-preserve-indentation nil)
  ;; Replace - with dot in lists
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1
             ()
           (compose-region (match-beginning 1) (match-end 1) "•"))))))
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

(leaf rust-mode :ensure t
  :hook (rust-mode . lsp))

(leaf cargo :ensure t
  :hook (rust-mode . cargo-minor-mode))

(leaf company-web :ensure t :after mhtml-mode)

(leaf ac-html-csswatcher :ensure t :after mhtml-mode)

(leaf mhtml-mode
  :when (>= emacs-major-version 26)
  :mode ("\\.vue\\'" "\\.html\\'" "\\.html\\'" "\\.jsx")
  :hook (mhtml-mode-hook . sgml-electric-tag-pair-mode)
  :config
  (setq mhtml-tag-relative-indent nil)
  (require 'company)                                   ; load company mode
  (require 'company-web-html)                          ; load company mode html backend
  ;; and/or
  (require 'company-web-jade)                          ; load company mode jade backend
  (require 'company-web-slim)                          ; load company mode slim backend
  (require 'ac-html-csswatcher)
  (company-web-csswatcher-setup)
  (define-key mhtml-mode-map (kbd "C-'") 'company-web-html)
  (add-hook 'mhtml-mode-hook (lambda ()
                             (set (make-local-variable 'company-backends) '(company-web-html company-files))
                             (company-mode t))))

(leaf web-mode :ensure t
  :when (< emacs-major-version 26)
  :mode ("\\.vue\\'" "\\.html\\'" "\\.htm\\'"))

(leaf elf-mode :ensure t)

(leaf cmake-mode :ensure t)

(leaf plantuml-mode :ensure t
  :when (locate-file "plantuml.jar" '("~/Downloads"))
  :init
  (setq plantuml-jar-path "~/Downloads/plantuml.jar"))

(leaf typescript-mode :ensure t)

(leaf treemacs :ensure t
  :bind ((treemacs-mode-map
          ([mouse-1] . treemacs-single-click-expand-action)))
  :hook (projectile-after-switch-project-hook . treemacs-display-current-project-exclusively)
  :config
  (treemacs-resize-icons 17))

(add-hook 'python-mode-hook (lambda () (setq-local fill-column 85)))
(leaf python
  :config
  (when (locate-file "ipython" exec-path)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt"))
  (setq python-indent-guess-indent-offset-verbose nil))

(leaf vterm :ensure t :when is-linux
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

(leaf tree-sitter :ensure t :require t :leaf-defer nil :disabled is-windows
  :preface
  (leaf tree-sitter-langs :ensure t :require t :leaf-defer nil)
  :config
  (require 'tree-sitter-hl)
  (require 'tree-sitter-debug)
  (require 'tree-sitter-query))

(leaf eshell-syntax-highlighting :ensure t :after esh-mode
  :config (eshell-syntax-highlighting-global-mode +1))

(leaf embrace :ensure t
  :bind
  ("C-," . embrace-commander))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(leaf ligature :ensure nil :require t :leaf-defer nil
  :disabled (or (< emacs-major-version 27) is-windows)
  :preface
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

(leaf volatile-highlights :ensure t :require t :leaf-defer nil
  :config
  (volatile-highlights-mode t))

(leaf diff-hl :ensure t
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode t)
  (diff-hl-margin-mode t)
  (diff-hl-flydiff-mode t)
  (diff-hl-dired-mode t))

(leaf whole-line-or-region :ensure t
  :config (whole-line-or-region-global-mode +1))

(defun after-init-jobs ()
  "Configurations run after Emacs starts."
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (when (> emacs-major-version 27)
    (set-face-attribute 'tab-bar-tab nil :box nil))
  (minions-mode 1)
  (setq debug-on-error  nil
        init-file-debug nil)
  (remove-hook 'after-init-hook 'after-init-jobs)
  (compro/redownload-empty-pkgs)

  ;; Remove text property from text in kill-ring
  (defun unpropertize-kill-ring ()
    (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
  (add-hook 'kill-emacs-hook 'unpropertize-kill-ring))
(add-hook 'after-init-hook 'after-init-jobs)
