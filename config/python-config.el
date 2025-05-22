(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(flycheck-define-checker
    python-mypy ""
    :command ("mypy"
              "--ignore-missing-imports"
              "--python-version" "3.6"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ": error:" (message) line-end))
    :modes python-mode)
(add-to-list 'flycheck-checkers 'python-mypy t)
(flycheck-add-next-checker 'python-pylint 'python-mypy t)

(use-package py-isort
  :after python
  :config (add-hook 'before-save-hook 'py-isort-before-save))

(use-package elpy
  :after python flycheck
  :config
  (elpy-enable)

  ;; should autoetect
  ;;   (setq elpy-rpc-python-command (expand-file-name "~/.pyenv/shims/python/"))

  ;; flycheck > flymake
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package blacken
  :after python
  :config (add-hook 'elpy-mode-hook 'blacken-mode))

(provide 'python-config)
