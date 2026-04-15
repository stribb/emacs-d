;;; auto-revert-merge.el --- Three-way merge for auto-revert  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; When a buffer has unsaved edits and the file changes on disk (e.g. an
;; external tool edited it), auto-revert normally refuses to revert.
;; This library intercepts that case and runs a three-way merge via diff3:
;;
;;   base  = buffer contents at last save/revert (snapshotted)
;;   local = current buffer contents (user's unsaved edits)
;;   remote = current file on disk (external edits)
;;
;; Clean merges are applied silently.  Conflicts activate smerge-mode
;; and optionally launch a hydra for resolution.

;;; Code:

(require 'cl-lib)

(defvar-local auto-revert-merge--base nil
  "Snapshot of buffer contents at last save or revert — the merge base.")

(defun auto-revert-merge--snapshot ()
  "Capture buffer contents as the merge base."
  (setq auto-revert-merge--base (buffer-substring-no-properties
                                  (point-min) (point-max))))

(defvar auto-revert-merge-conflict-hook nil
  "Hook run after a conflicting merge, with smerge-mode active.
Use this to launch a merge UI (e.g. hydra, transient).")

(defun auto-revert-merge ()
  "Three-way merge when auto-revert finds a modified buffer with disk changes.
Returns non-nil if the merge was attempted, nil to fall through to
the normal auto-revert handler."
  (when (and buffer-file-name
             (buffer-modified-p)
             (file-exists-p buffer-file-name)
             (not (verify-visited-file-modtime))
             auto-revert-merge--base)
    (let ((base-contents auto-revert-merge--base)
          (base (make-temp-file "merge-base-"))
          (local (make-temp-file "merge-local-"))
          (result (make-temp-file "merge-result-"))
          (pt (point)))
      (unwind-protect
          (progn
            (with-temp-file base
              (insert base-contents))
            (write-region nil nil local nil 'silent)
            (let ((exit (call-process "diff3" nil (list :file result) nil
                                      "-m" local base buffer-file-name)))
              (let ((merged (with-temp-buffer
                              (insert-file-contents result)
                              (buffer-string))))
                (erase-buffer)
                (insert merged)
                (goto-char (min pt (point-max)))
                (set-visited-file-modtime)
                (auto-revert-merge--snapshot)
                (if (= exit 0)
                    (message "Auto-merged disk changes cleanly")
                  (smerge-mode 1)
                  (run-hooks 'auto-revert-merge-conflict-hook)
                  (message "Merge conflicts — resolve with smerge")))))
        (delete-file base)
        (delete-file local)
        (delete-file result)))
    t))

(defun auto-revert-merge--advice (orig &rest args)
  "Around advice for `auto-revert-handler'.
When the buffer is modified and the file changed on disk, attempt a
three-way merge instead of skipping the revert."
  (if (and buffer-file-name
           (buffer-modified-p)
           (not (verify-visited-file-modtime)))
      (auto-revert-merge)
    (apply orig args)))

;;;###autoload
(define-minor-mode auto-revert-merge-mode
  "Automatically three-way merge when a modified buffer's file changes on disk."
  :global t
  :lighter nil
  (if auto-revert-merge-mode
      (progn
        (add-hook 'after-save-hook #'auto-revert-merge--snapshot)
        (add-hook 'after-revert-hook #'auto-revert-merge--snapshot)
        (add-hook 'find-file-hook #'auto-revert-merge--snapshot)
        (advice-add 'auto-revert-handler :around #'auto-revert-merge--advice))
    (remove-hook 'after-save-hook #'auto-revert-merge--snapshot)
    (remove-hook 'after-revert-hook #'auto-revert-merge--snapshot)
    (remove-hook 'find-file-hook #'auto-revert-merge--snapshot)
    (advice-remove 'auto-revert-handler #'auto-revert-merge--advice)))

(provide 'auto-revert-merge)
;;; auto-revert-merge.el ends here
