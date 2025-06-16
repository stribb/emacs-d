(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(use-package python-ts-mode
  :straight nil
  :if (treesit-language-available-p 'python) ; Only if python grammar is usable
  :mode ("\\.py\\'" . python-ts-mode)
  :hook (python-ts-mode-hook . (lambda () (setq fill-column 88))))

(provide 'python-config)
