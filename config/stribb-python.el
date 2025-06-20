;;; package --- stribb-python -*- lexical-binding: nil; -*-

;;; Commentary:
;;; This file contains Python config, excluding eglot

;;; Code:

;;; stribb-ui.el ends here

(use-package python
  :preface
  (cl-assert (treesit-language-available-p 'python))
  :straight nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook ( python-ts-mode-hook .
	  (lambda ()
	    (unless (local-variable-p 'fill-column (setq fill-column 100))))))

(provide 'stribb-python)
;;; stribb-python.el ends here
