(setq-default
 ;;;   Use spaces and not tabs for indentation
 indent-tabs-mode nil

 ;;;   Don't highlight trailing whitespaces by default
 show-trailing-whitespace nil

 ;;;   Org
 org-src-fontify-natively t ;; Fontify source blocks
 )

(setq
 ;;;   Initial major mode for *scratch* buffer
 initial-major-mode 'fundamental-mode

 ;;;   Node.js path from nvm
 exec-path (append exec-path '("/home/compro/.nvm/versions/node/v9.3.0/bin/"))

 ;;;   User details
 user-mail-address "comproprasad@gmail.com"
 user-full-name    "Abhishek(Compro) Prasad"

 ;;;   Only use ~/.authinfo.gpg
 auth-sources (list (expand-file-name "~/.authinfo.gpg"))

 ;;;   Security settings
 gnutls-verify-error t

 ;;;   Customizations go to this file
 custom-file (expand-file-name "custom.el" user-emacs-directory)

 ;;;   Follow symlinks to the actual file
 find-file-visit-truename t
 vc-follow-symlinks t

 ;;;   Jump by words separated by punctuations
 global-subword-mode t

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

 ;;;   GC triggers per 7 MB increase in memory
 gc-cons-threshold 58720256

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

 ;;;   Save existing interprogram clipboard text before replacing it
 save-interprogram-paste-before-kill t

 ;;;   Set REPL programs' prompt as read only
 comint-prompt-read-only t

 ;;;   Use commands when in in minibuffer
 enable-recursive-minibuffers t

 ;;;   Scroll output in *compilation* buffer
 compilation-scroll-output t

 ;;;   Scroll one line at a time no matter what
 scroll-step            1
 scroll-conservatively  10000
 mouse-wheel-scroll-amount '(1 ((shift) . 1))

 ;;;   Remember screen position after scrolling
 scroll-preserve-screen-position 'always

 ;;;   Initial scratch message is nil
 initial-scratch-message ""

 ;;;   Use directory local variables in tramp session
 enable-remote-dir-locals t

 ;;;   Backup configuration
 tramp-persistency-file-name "~/.emacs.d/.cache/tramp"
 backup-directory-alist '(("." . "~/.emacs.d/.cache/backups"))
 delete-old-versions -1
 version-control t
 vc-make-backup-files t
 auto-save-file-name-transforms '((".*" "~/.emacs.d/.cache/auto-save-list" t))
 auto-save-list-file-prefix "~/.emacs.d/.cache/auto-save-list/saves-"

 ;;;   ERC configurations
 erc-hide-list '("PART" "QUIT" "JOIN")
 erc-server    "107.182.226.199"  ;;; IP for "irc.freenode.net"
 erc-nick      "compro"

 ;;;   Dired
 dired-dwim-target t

 ;;;   Ediff
 ediff-window-setup-function 'ediff-setup-windows-plain ;; Single frame ediff session

 ;;;   Org mode configurations
 org-startup-indented t
 org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
 org-ellipsis " "              ;; folding symbol
 org-pretty-entities t
 org-hide-emphasis-markers t    ;; show actually italicized text instead of /italicized text/
 org-agenda-block-separator ""
 org-fontify-whole-heading-line t
 org-fontify-done-headline t
 org-fontify-quote-and-verse-blocks t
 org-default-notes-file "/home/compro/Dropbox/programs/notes/notes.org"
 org-todo-keywords '((sequence "TODO(t)" "inPROGRESS(i)" "|" "DONE(d)" "CANCELED(c)"))

 ;;;   Ido mode
 ido-enable-flex-matching t
 ido-save-directory-list-file "~/.emacs.d/.cache/ido.last"

 ;;;   Line numbers
 display-line-numbers-type 'relative
 )

(with-eval-after-load 'ox-latex
  ;;;   Set colors when exporting to latex
  (setq org-latex-listings t))

(fset 'yes-or-no-p 'y-or-n-p)

(blink-cursor-mode 0)

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

(set-frame-font "Source Code Pro-10")

(menu-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-no-scroll-bar)

(column-number-mode 1)
(display-time-mode 1)

(delete-selection-mode 1)

(if (not window-system)
    (xterm-mouse-mode 1)
  (xterm-mouse-mode 0))

(toggle-frame-maximized)

(when (display-graphic-p)
  (general-define-key
   :keymaps 'input-decode-map
   [?\C-m] [C-m]
   [?\C-i] [C-i]
   [?\C-j] [C-j]
   [?\C-\[] (kbd "<C-[>")))

(general-define-key
 [down]            nil
 [up]              nil
 [left]            nil
 [right]           nil
 "C-z"             nil
 "C-x C-o"         'ff-find-other-file
 [C-m]             'delete-other-windows
 "<C-tab>"         'previous-buffer
 "<C-iso-lefttab>" 'next-buffer
 "<C-backtab>"     'next-buffer
 "C-z <tab>"       'toggle-minibuffer-message-timer
 "<C-S-mouse-1>"   'imenu
 "C-<f4>"          'kill-current-buffer
 "M-/"             'hippie-expand
 [mouse-3]         menu-bar-edit-menu
 "M-^"             'compile)

;;;   Automatically change to newest version of file if edited externally
(global-auto-revert-mode t)

;;;   Highlight matching pairs like (), {}, [], etc.
(show-paren-mode t)

;;;   Enable line numbers in fundamental mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)



(defun my/set-show-whitespace-mode ()
  "Show white space in current buffer"
  (setq show-trailing-whitespace t))
;; Show whitespaces only in buffers pointing to specific files
(add-hook 'find-file-hook 'my/set-show-whitespace-mode)
;; Remove the trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(with-eval-after-load 'abbrev
  (progn
    (when (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
    (when (file-exists-p "./my-abbrev.el")
      (load "./my-abbrev.el"))))

;;;   Highlighting current line is improtant when lines are long in tables
(add-hook 'package-menu-mode-hook 'hl-line-mode)

(when (fboundp 'xwidget-webkit-browse-url)
  (use-package xwidget
    :general
    (:keymaps 'xwidget-webkit-mode-map
              "<mouse-4>" 'xwidget-webkit-scroll-down
              "<mouse-5>" 'xwidget-webkit-scroll-up
              "<up>" 'xwidget-webkit-scroll-down
              "<down>" 'xwidget-webkit-scroll-up
              "M-w" 'xwidget-webkit-copy-selection-as-kill
              "C-c" 'xwidget-webkit-copy-selection-as-kill)
    :hook
    (window-configuration-change-hook
     . (lambda ()
         (when (equal major-mode 'xwidget-webkit-mode)
           (xwidget-webkit-adjust-size-dispatch))))
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
  )


(defcustom minibuffer-message-list
  '("Let the hacking begin!"
    "Welcome to Emacs!"
    "Have a good day!"
    "Good luck configuring Emacs!"
    "Better concentrate on your work"
    "Don’t even think about other editors"
    "Are you sleeping?"
    "Sorry, if I broke your concentration!"
    "Please update me"
    "The minibuffer sucks!"
    "Remember the day when we first met?"
    "Did you stop typing?"
    "You should commit your changes before you mess up"
    "I am on a loop"
    "You should keep a log of your tasks"
    "Please don’t quit! Please!"
    "Did you call your Mom?"
    "I am older than you"
    "Don’t get into the XY problem"
    "Get some rest")
  "List of messages that are displayed in the minibuffer after a specific period
of time controlled by `minibuffer-message-display-interval'."
  :type '(repeat string))


(defvar minibuffer-message-display-interval 5
  "Time in minutes after which a random message from `minibuffer-message-list'
is shown in minibuffer.")


(defvar minibuffer-message-echo-timer nil
  "Object that stores the timer for messages that are displayed in the
minibuffer using `display-startup-echo-area-message'.")


(defun restart-minibuffer-message-display-timer (func)
  "Start the minibuffer timer with `FUNC' running per
`minibuffer-message-display-interval'."
  (when minibuffer-message-echo-timer (cancel-timer minibuffer-message-echo-timer))
  (setq minibuffer-message-echo-timer
        (run-with-idle-timer (* minibuffer-message-display-interval 60) t func)))


(defun display-startup-echo-area-message ()
  "Show a message in minibuffer."
  (interactive)
  (message
   (let* ((length (length minibuffer-message-list))
          (random-number (abs (% (random) length))))
     (car (nthcdr random-number minibuffer-message-list)))))


(restart-minibuffer-message-display-timer 'display-startup-echo-area-message)


(defun toggle-minibuffer-message-timer ()
   "Toggle minibuffer message showing per
`minibuffer-message-display-interval'."
   (interactive)
   (if (null minibuffer-message-echo-timer)
       (restart-minibuffer-message-display-timer 'display-startup-echo-area-message)
     (cancel-timer minibuffer-message-echo-timer)
     (setq minibuffer-message-echo-timer)))


(setq recentf-max-saved-items 512
      history-length t
      history-delete-duplicates t
      recentf-save-file "~/.emacs.d/.cache/recentf"
      savehist-file "~/.emacs.d/.cache/savehist"
      save-place-file "~/.emacs.d/.cache/saveplace"
      savehist-additional-variables '(kill-ring
				      extended-command-history
				      global-mark-ring
				      mark-ring
				      regexp-search-ring
				      search-ring))
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)

(defun my-comint-preoutput-read-only (text)
  (propertize text 'read-only t))

(add-hook 'comint-preoutput-filter-functions
          'my-comint-preoutput-read-only)

(defun my-comint-last-output-beg ()
  (save-excursion
    (comint-goto-process-mark)
    (while (not (or (eq (get-char-property (point) 'field) 'boundary)
                    (= (point) (point-min))))
      (goto-char (previous-char-property-change (point) (point-min))))
    (if (= (point) (point-min))
        (point)
      (1+ (point)))))

(defun my-comint-last-output-end ()
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

(defun my-comint-clear-last-output ()
  (interactive)
  (let ((start (my-comint-last-output-beg))
        (end (my-comint-last-output-end)))
    (let ((inhibit-read-only t))
      (delete-region start end)
      (save-excursion
        (goto-char start)
        (insert (propertize "output cleared"
                            'font-lock-face 'font-lock-comment-face))))))

(defun my-shell-kill-buffer-sentinel (process event)
  (when (and (memq (process-status process) '(exit signal))
             (buffer-live-p (process-buffer process)))
    (kill-buffer)))

(defun my-kill-process-buffer-on-exit ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'my-shell-kill-buffer-sentinel))

(dolist (hook '(ielm-mode-hook term-exec-hook comint-exec-hook))
  (add-hook hook 'my-kill-process-buffer-on-exit))

(defun my-kill-word (arg)
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

;;;   Switch to file buffers using next-buffer and previous-buffer
(defun compro/files-buffer-predicate (buffer)
  (stringp (buffer-file-name buffer)))
(set-frame-parameter nil 'buffer-predicate 'compro/files-buffer-predicate)
;;;   end

(defun compro/smarter-backward-kill-word ()
  "Deletes the previous word, respecting:
1. If the cursor is at the beginning of line, delete the '\n'.
2. If there is only whitespace, delete only to beginning of line.
3. If there is whitespace, delete whitespace and check 4-5.
4. If there are other characters instead of words, delete one only char.
5. If it's a word at point, delete it."
  (interactive)

  (if (bolp)
      ;; 1
      (delete-char -1)

    (if (string-match-p "^[[:space:]]+$"
                        (buffer-substring-no-properties
                         (line-beginning-position) (point)))
        ;; 2
        (delete-horizontal-space)

      (when (thing-at-point 'whitespace)
        ;; 3
        (delete-horizontal-space))

      (if (thing-at-point 'word)
          ;; 5
          (let ((start (car (bounds-of-thing-at-point 'word)))
                (end (point)))
            (if (> end start)
                (delete-region start end)
              (delete-char -1)))
        ;; 4
        (delete-char -1)))))

(with-eval-after-load 'comint
  (general-define-key
   :kemaps 'comint-mode-map
   "<remap> <kill-word>" 'my-kill-word
   "<remap> <backward-kill-word>" 'compro/smarter-backward-kill-word
   "C-S-l" 'my-comint-clear-last-output))

(general-define-key
 :keymaps 'global-map
 "<remap> <backward-kill-word>" 'compro/smarter-backward-kill-word)

(defun my-shell-turn-echo-off ()
  (setq comint-process-echoes t))

(add-hook 'shell-mode-hook 'my-shell-turn-echo-off)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
	try-expand-all-abbrevs
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-dabbrev
	try-expand-dabbrev-from-kill
	try-expand-dabbrev-all-buffers
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

(minibuffer-depth-indicate-mode 1)

(use-package winner
  :init
  (winner-mode 1))

(add-hook 'focus-out-hook #'garbage-collect)

;; Keep a single window after startup
(add-hook 'window-setup-hook #'delete-other-windows)

(global-prettify-symbols-mode 1)
(add-hook 'python-mode-hook 'python-prettify-symbols)
(defun python-prettify-symbols ()
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        '(;; Syntax
          ("def" .      #x2131)
          ("not" .      #x2757)
          ("in" .       #x2208)
          ("not in" .   #x2209)
          ("return" .   #x27fc)
          ("yield" .    #x27fb)
          ("for" .      #x2200)
          ;; conditions
          ("!=" .       #x2260)
          ("<=" .       #x2264)
          (">=" .       #x2265)
          ;; Base Types
          ("int" .      #x2124)
          ("float" .    #x211d)
          ("str" .      #x1d54a)
          ("True" .     #x1d54b)
          ("False" .    #x1d53d)
          )))

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

(set-default-font '("Ubuntu Mono"
                    :size 16
                    :weight normal
                    :width normal))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize the compilation buffer with ANSI escape sequences."
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun my-org-autodone (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'my-org-autodone)


;;;   Fontify exported PDF using minted
;; Include the latex-exporter
(require 'ox-latex)
;; Add minted to the defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; Tell the latex export to use the minted package for source
;; code coloration.
(setq org-latex-listings 'minted)
;; Let the exporter use the -shell-escape option to let latex
;; execute external programs.
;; This obviously and can be dangerous to activate!

;; I use pdflatex instead of xelatex because that seems to work
;; much better with utf-8 files
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;;   end


;;;   Publish using org mode
(setq org-publish-website-local "~/Downloads/github.com/Compro-Prasad/website/")
(setq org-publish-project-alist
      `(("orgfiles"
         :auto-sitemap t
         :sitemap-title "List of pages"
         :base-directory ,(concat org-publish-website-local "org")
         :base-extension "org"
         :publishing-directory ,(concat org-publish-website-local "html")
         :publishing-function org-html-publish-to-html
         ;; :exclude "PrivatePage.org"   ;; regexp
         ;; :headline-levels 3
         :section-numbers nil
         :with-toc 1
         :html-head "<link rel=\"stylesheet\"
                       href=\"../css/mystyle.css\" type=\"text/css\"/>"
         :html-preamble t)
        ("website" :components ("orgfiles"))))
;;;   end


;; No box around modeline
(defun after-init-jobs ()
  "Configurations run after Emacs starts."
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (remove-hook 'after-init-hook 'after-init-jobs))

(add-hook 'after-init-hook 'after-init-jobs)
