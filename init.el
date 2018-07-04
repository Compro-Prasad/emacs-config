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


;;;   Tell use-package to always use straight.el when installing packages
(setq straight-use-package-by-default t)
;;;   end
