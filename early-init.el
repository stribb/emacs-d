;;; package --- early-init -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Just the thing we need to do before init happens in earnest.
;; We're putting experimental flags here.

;;; Code:
(setq package-enable-at-startup nil)

;;; Experimental Features System
;; Feature flags for testing modern alternatives and removing legacy workarounds
(defvar stribb-exp-alist
  nil
  "Feature flags for experimental configuration changes.")

(defun define-feature (name default doc)
  "Define a feature NAME with DEFAULT value and DOC string."
  ;; Update feature alist
  (let ((entry (assq name stribb-exp-alist)))
    (if entry
        (setcdr entry default)
      (push (cons name default) stribb-exp-alist)))
  ;; Attach documentation to symbol
  (put name 'feature-documentation doc)
  name)

(defun stribb-exp-parse (switch)
  "Set exp to a value based on SWITCH argument.

If it starts with + or has no value, enable it;
starts with - and has no value, disable it
else sets it to val."
  ;; Pop the next argument from command-line-args-left
  (let* ((feature-str (pop command-line-args-left))
         (prefix (substring feature-str 0 1))
         (has-prefix (member prefix '("+" "-")))
         (name-part (if has-prefix
                        (substring feature-str 1)
                      feature-str))
         (parts (split-string name-part "=" t))
         (feature (intern (car parts)))
         (val (cond
               ;; Has explicit value after =
               ((cdr parts) (read (cadr parts)))
               ;; Starts with + or no prefix (default enable)
               ((or (string= prefix "+") (not has-prefix)) t)
               ;; Starts with -
               ((string= prefix "-") nil)))
         (entry (assq feature stribb-exp-alist)))
    (if entry
        (setcdr entry val)
      ;; Print to stderr and kill Emacs immediately on unknown feature
      (progn
        (princ (format "Error: Unknown experimental feature: %s\n" feature) #'external-debugging-output)
        (kill-emacs 1)))))

(add-to-list 'command-switch-alist '("--exp" . stribb-exp-parse))

(defun stribb-feature-enabled-p (feature)
  "Check if FEATURE is enabled in experimental features."
  (cdr (assq feature stribb-exp-alist)))

(define-feature 'helm-native-comp nil
		"Remove Helm native compilation workaround.")


;;; Old fogey mode.
(let ((inhibit-message t))
  (message "Disabling fogeyphobic UI elements..."))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(provide 'early-init)
;;; early-init.el ends here
