;;; vc-jj.el --- VC backend for Jujutsu -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal vc backend shim for Jujutsu (jj).  Just enough for
;; `vc-root-dir' and `vc-mode' to work; full vc operations are not
;; yet supported.

;;; Code:

(defun vc-jj--root (dir)
  "Return the jj repo root for DIR, or nil."
  (let ((default-directory dir))
    (ignore-errors
      (file-truename
       (string-trim (shell-command-to-string "jj root 2>/dev/null"))))))

(defun vc-jj-root (file)
  "Return the jj repo root containing FILE."
  (vc-jj--root (if (file-directory-p file)
                    file
                  (file-name-directory file))))

(defun vc-jj-registered (file)
  "Return non-nil if FILE is in a jj repository."
  (not (null (vc-jj-root file))))

(defun vc-jj-state (_file)
  "Return the vc state of FILE.  Always `up-to-date' for now."
  'up-to-date)

(defun vc-jj-working-revision (_file)
  "Return the working revision.  Placeholder."
  "jj")

(provide 'vc-jj)
;;; vc-jj.el ends here
