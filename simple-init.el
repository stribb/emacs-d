;; Use emacs -Q -l ~/.emacs.d/simple-init.el

(load "~/.emacs.d/straight/repos/straight.el/bootstrap.el")

(use-package straight
  :custom
  ;; https://github.com/radian-software/straight.el/issues/1146
  (straight-use-package-by-default t)
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode flymake xref)))
(straight-use-package 'solarized-theme)

(use-package solarized-theme
  :config
  (setq custom-enabled-themes '(solarized-dark solarized-light)
        solarized-high-contrast-mode-line t)
  (load-theme 'solarized-light t))

(use-package helm
  :delight
  :after (async popup)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("<C-return>" . helm-find-files)
         ("s-t" . helm-find-files)
         ("C-x C-v" . find-alternate-file)
         ("C-x C-d" . helm-browse-project)
         ("C-x b" . helm-buffers-list)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-r" . helm-recentf)
         ("C-h a" . helm-apropos)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :demand t
  :config
  (defun stribb/helm-eshell-completions nil
    (eshell-cmpl-initialize)
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
  (add-hook 'eshell-mode-hook #'stribb/helm-eshell-completions)
  (helm-mode 1))

(use-package num3-mode
  :config
  (global-num3-mode))

(use-package magit)
