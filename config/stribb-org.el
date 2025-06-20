;;; org-config --- andstrib's org-mode configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; From experience, org-mode config tends to overwhelm init.el, so
;; here we're factoring it out.

;;; Code:

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (R . t)
   (python . t)))

(setq-default org-confirm-babel-evaluate nil)

(setq-default org-export-backends '(ascii html latex md))
;; Experimenting with visual-line-mode for text-modes.
;; (add-hook 'org-mode-hook #'turn-on-auto-fill)

(provide 'stribb-org)
;;; stribb-org.el ends here
