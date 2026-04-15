;;; stribb-startup.el --- Startup performance and desktop restart  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Startup lifecycle: GC tuning, file-name-handler suppression, and
;; desktop-based restart/quit.  Loaded from init.el before anything else.

;;; Code:

(defvar file-name-handler-alist-old file-name-handler-alist
  "Saved value of `file-name-handler-alist' before startup suppression.")

(defun stribb/optimize-startup ()
  "Suppress expensive runtime hooks for faster init.
Call `stribb/restore-vars' via `after-init-hook' to undo."
  (setq debug-on-error t
        file-name-handler-alist nil
        message-log-max 16384
        gc-cons-threshold 4026531840
        gc-cons-percentage 10.5
        auto-window-vscroll nil
        inhibit-startup-screen t))

(defun stribb/restore-vars ()
  "Restore variables suppressed by `stribb/optimize-startup'."
  (setq debug-on-error nil
        file-name-handler-alist file-name-handler-alist-old
        gc-cons-threshold 800000
        gc-cons-percentage 0.1)
  (run-with-idle-timer 10 nil #'garbage-collect)
  (run-with-timer 0.1 nil #'stribb/maybe-restore-desktop))

;;; Desktop restart

(defvar stribb/restart-desktop-file ".emacs-restart-desktop"
  "Desktop file that also serves as restart flag in `user-emacs-directory'.")

(defun stribb/maybe-restore-desktop ()
  "Check if this was a restart and restore desktop if so."
  (let ((desktop-file (expand-file-name stribb/restart-desktop-file user-emacs-directory)))
    (when (file-exists-p desktop-file)
      (letrec ((cleanup-fn
                (lambda ()
                  (when (file-exists-p desktop-file)
                    (delete-file desktop-file))
                  (remove-hook 'desktop-after-read-hook cleanup-fn))))
        (add-hook 'desktop-after-read-hook cleanup-fn))
      (dlet ((desktop-base-file-name stribb/restart-desktop-file))
        (desktop-read user-emacs-directory)))))

(defun stribb/save-desktop-state ()
  "Internal helper to save the current desktop state."
  (dlet ((desktop-base-file-name stribb/restart-desktop-file))
    (desktop-save user-emacs-directory t)))

(defun stribb/restart-emacs ()
  "Save the current desktop and restart Emacs."
  (interactive)
  (stribb/save-desktop-state)
  (restart-emacs))

(defun stribb/quit-emacs ()
  "Save the current desktop and quit Emacs."
  (interactive)
  (stribb/save-desktop-state)
  (save-buffers-kill-emacs))

(defalias 'rrr 'stribb/restart-emacs)
(defalias 'qqq 'stribb/quit-emacs)

(provide 'stribb-startup)
;;; stribb-startup.el ends here
