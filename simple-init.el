;; Use emacs -Q -l ~/.emacs.d/simple-init.el

(load "~/.emacs.d/straight/repos/straight.el/bootstrap.el")
(load "~/.emacs.d/straight/repos/use-package/bind-key.el")

(straight-use-package 'solarized-theme)

(setq custom-enabled-themes '(solarized-dark solarized-light)
      solarized-high-contrast-mode-line t)
(load-theme 'solarized-light t)

(straight-use-package 'helm)

(require 'helm-config)
(helm-mode 1)
(bind-keys ("M-x" . helm-M-x)
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

(straight-use-package 'num3-mode)
(global-num3-mode)
