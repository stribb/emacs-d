;;; org-config --- andstrib's org-mode configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; From experience, org-mode config tends to overwhelm init.el, so
;; here we're factoring it out.

;;; Code:
(require 'use-package)
(require 'yasnippet)

(defun stribb/org-daily-note ()
  "Create and/or find the daily notes file, setting point to the end."
  (find-file (expand-file-name
                    (concat "journal/" (format-time-string "%Y-%m-%d") ".org")
                    org-directory))
  (when (= (point-min) (point-max))
    (yas-expand-snippet (yas-lookup-snippet "journal")))
  (goto-char (point-max)))

(use-package org
  :straight nil
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c C-x i" . org-clock-in)
         ("C-c C-x j" . org-clock-goto)
         ("C-c C-x o" . org-clock-out)
         ("C-c c" . org-capture))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (R . t)
     (python . t)))
  (setq org-directory (expand-file-name "~/org"))
  (setq org-capture-templates
        '(("n" "Daily notes" entry (function stribb/org-daily-note) "* %?\n %T")
          ("c" "Daily notes (contextual)" entry (function stribb/org-daily-note) "* %?\n %T %a")))
  (setq org-confirm-babel-evaluate nil)
  (setq org-export-backends '(ascii html latex md)))

(use-package htmlize
  :after org)

(use-package org-pomodoro
  :after org
  :defines org-pomodoro-length org-pomodoro-end-time
  :bind (("C-c C-x C-i" . org-pomodoro))
  :config
  (add-hook 'org-pomodoro-started-hook #'stribb/org-pomodoro-focus))

(defun stribb/org-pomodoro-focus ()
  "Prompt the user for their state of mind before starting a pomodoro."
  (run-at-time
   "0 sec" nil
   (lambda ()
     (let* ((state (read-char-choice
                    "State of mind (distracted, meh, good, flow): "
                    (string-to-list "dmgf")))
            (mod (- (cl-case state
                       ("distracted" 5)
                       ("meh" 10)
                       ("good" 25)
                       ("flow" 45))
                    org-pomodoro-length)))
       (setq org-pomodoro-end-time (time-add org-pomodoro-end-time (* 60 mod)))
       (message "You are feeling %s so %d minutes were %s your pomodoro"
                state (abs mod) (if (>= mod 0) "added to" "removed from"))))))


(provide 'stribb-org)
;;; stribb-org.el ends here
