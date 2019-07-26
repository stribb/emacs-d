;;; org-config --- andstrib's org-mode configuration

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

;; Experimenting with visual-line-mode for text-modes.
;; (add-hook 'org-mode-hook #'turn-on-auto-fill)

(provide 'org-config)
;;; org-config.el ends here
