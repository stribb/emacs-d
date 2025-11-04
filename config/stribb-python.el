;;; package --- stribb-python -*- lexical-binding: nil; -*-

;;; Commentary:
;;; This file contains Python config, excluding eglot

;;; Code:

(defun stribb/format-with-ruff ()
  "Format current buffer with ruff if available."
  (when (and (executable-find "ruff")
             buffer-file-name
             (derived-mode-p 'python-mode 'python-ts-mode))
    (let ((output-buffer (get-buffer-create "*ruff-format*")))
      (if (zerop (call-process "ruff" nil output-buffer nil
                               "format" buffer-file-name))
          (revert-buffer t t t)
        (message "Ruff formatting failed. Check *ruff-format* buffer.")))))

(use-package python
  :preface
  (unless (treesit-language-available-p 'python)
    (warn "Python tree-sitter grammar not available. Install with M-x treesit-install-language-grammar"))
  :straight nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook ((python-ts-mode-hook python-mode-hook) .
	 (lambda ()
	   (add-hook 'before-save-hook #'stribb/format-with-ruff nil 'local))))

(provide 'stribb-python)
;;; stribb-python.el ends here
