;;; project-idle-cleanup.el --- Close inactive project frames -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Abhishek Prasad
;; Author: Abhishek Prasad
;; Keywords: convenience, project
;; Version: 1.0

;; This file is part of the author's Emacs configuration and is free software:
;; you can redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation, either version
;; 3 of the License, or (at your option) any later version.

;;; Commentary:

;; Automatically kill buffers and close frames for inactive project.el
;; projects.  Project frames are identified by their `compro-project-root'
;; frame parameter, which is assigned by the project-frame configuration.
;;
;; Modified buffers and buffers with live processes prevent cleanup.  A regular
;; Emacs session's last frame is preserved and detached from the project.
;;
;; Enable `compro/project-idle-cleanup-mode' to start tracking and cleanup.

;;; Code:

(require 'project)
(require 'seq)
(require 'time-date)

(defgroup compro-project-idle-cleanup nil
  "Automatically clean up inactive project frames."
  :group 'project)

(defcustom compro/project-idle-timeout (* 12 60 60)
  "Seconds a project can remain inactive before cleanup."
  :type 'integer
  :group 'compro-project-idle-cleanup)

(defcustom compro/project-idle-check-interval (* 5 60)
  "Seconds between checks for inactive project frames."
  :type '(integer 1 nil)
  :group 'compro-project-idle-cleanup)

(defvar compro/project-idle-cleanup-timer nil
  "Timer that closes inactive project frames.")

(defun compro/set-project-frame-root (frame root)
  "Assign project ROOT to FRAME and initialize its idle timestamp."
  (unless (equal root (frame-parameter frame 'compro-project-root))
    (set-frame-parameter frame 'compro-project-root root)
    (set-frame-parameter frame 'compro-project-last-active (current-time)))
  (unless (frame-parameter frame 'compro-project-last-active)
    (set-frame-parameter frame 'compro-project-last-active (current-time))))

(defun compro/mark-project-frame-active (&optional frame)
  "Record user activity in project FRAME, defaulting to the selected frame."
  (let ((frame (or frame (selected-frame))))
    (when (and (frame-live-p frame)
               (frame-parameter frame 'compro-project-root))
      (set-frame-parameter frame 'compro-project-last-active (current-time)))))

(defun compro/mark-focused-project-frames-active ()
  "Record activity for project frames Emacs knows are focused."
  (let ((now (current-time)))
    (dolist (frame (frame-list))
      (when (and (frame-parameter frame 'compro-project-root)
                 (fboundp 'frame-focus-state)
                 (eq t (frame-focus-state frame)))
        (set-frame-parameter frame 'compro-project-last-active now)))))

(defun compro/project-buffer-prevents-idle-cleanup-p (buffer)
  "Return non-nil if BUFFER has unsaved changes or a running process."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (or (buffer-modified-p)
             (let ((process (get-buffer-process buffer)))
               (and process (process-live-p process)))))))

(defun compro/project-root-active-p (root now)
  "Return non-nil if any frame for ROOT was active within the timeout."
  (seq-some
   (lambda (frame)
     (let ((last-active (frame-parameter frame 'compro-project-last-active)))
       (and (equal root (frame-parameter frame 'compro-project-root))
            last-active
            (< (float-time (time-subtract now last-active))
               compro/project-idle-timeout))))
   (frame-list)))

(defun compro/project-frame-has-sibling-on-terminal-p (frame)
  "Return non-nil if another regular frame shares FRAME's terminal."
  (seq-some
   (lambda (other-frame)
     (and (frame-live-p other-frame)
          (not (eq frame other-frame))
          (not (eq 'only (frame-parameter other-frame 'minibuffer)))
          (eq (frame-terminal frame) (frame-terminal other-frame))))
   (frame-list)))

(defun compro/close-inactive-project-frame (frame root)
  "Kill ROOT's buffers and close its inactive FRAME when it is safe."
  (let ((project (project-current nil root)))
    (when project
      (let ((buffers (project-buffers project)))
        (unless (seq-some #'compro/project-buffer-prevents-idle-cleanup-p
                          buffers)
          (condition-case error-data
              (progn
                (dolist (buffer buffers)
                  (when (buffer-live-p buffer)
                    (kill-buffer buffer)))
                (if (seq-some #'buffer-live-p buffers)
                    (progn
                      ;; Avoid retrying a buffer that refused to die every few
                      ;; minutes; give it another full timeout instead.
                      (set-frame-parameter frame 'compro-project-last-active
                                           (current-time))
                      (message "Kept inactive project %s: a buffer refused to close"
                               root))
                  (when (frame-live-p frame)
                    (if (or (daemonp)
                            (compro/project-frame-has-sibling-on-terminal-p
                             frame))
                        (delete-frame frame)
                      ;; Deleting the only frame would close a regular Emacs
                      ;; session, so leave it as an unassigned empty frame.
                      (set-frame-parameter frame 'compro-project-root nil)
                      (set-frame-parameter frame 'compro-project-last-active nil)
                      (modify-frame-parameters
                       frame '((name . "Emacs") (title . "Emacs"))))
                    (message "Cleaned up inactive project %s" root))))
            (error
             (when (frame-live-p frame)
               (set-frame-parameter frame 'compro-project-last-active
                                    (current-time)))
             (message "Could not clean up inactive project %s: %s"
                      root (error-message-string error-data)))))))))

(defun compro/cleanup-inactive-project-frames ()
  "Kill buffers and close project frames inactive past the timeout."
  (let ((now (current-time)))
    (dolist (frame (frame-list))
      (let ((root (frame-parameter frame 'compro-project-root))
            (last-active
             (frame-parameter frame 'compro-project-last-active)))
        (when (and root last-active
                   (>= (float-time (time-subtract now last-active))
                       compro/project-idle-timeout)
                   (not (compro/project-root-active-p root now)))
          (compro/close-inactive-project-frame frame root))))))

(define-minor-mode compro/project-idle-cleanup-mode
  "Track project frame activity and clean up expired projects."
  :global t
  :group 'compro-project-idle-cleanup
  (if compro/project-idle-cleanup-mode
      (progn
        (add-hook 'post-command-hook #'compro/mark-project-frame-active)
        (when (boundp 'after-focus-change-function)
          (add-function :after after-focus-change-function
                        #'compro/mark-focused-project-frames-active))
        (when (timerp compro/project-idle-cleanup-timer)
          (cancel-timer compro/project-idle-cleanup-timer))
        (setq compro/project-idle-cleanup-timer
              (run-at-time compro/project-idle-check-interval
                           compro/project-idle-check-interval
                           #'compro/cleanup-inactive-project-frames)))
    (remove-hook 'post-command-hook #'compro/mark-project-frame-active)
    (when (boundp 'after-focus-change-function)
      (remove-function after-focus-change-function
                       #'compro/mark-focused-project-frames-active))
    (when (timerp compro/project-idle-cleanup-timer)
      (cancel-timer compro/project-idle-cleanup-timer))
    (setq compro/project-idle-cleanup-timer nil)))

(provide 'project-idle-cleanup)
;;; project-idle-cleanup.el ends here
