;;; init.el --- Emacs startup orchestrator  -*- lexical-binding: t; -*-

;;; Commentary:
;; Lifecycle: startup perf → built-in modes → straight → use-package → modules.

;;; Code:

;; Load-path: config modules and local libraries.
(dolist (d '("config" "lisp"))
  (add-to-list 'load-path (expand-file-name d user-emacs-directory)))

;; Startup performance — must run before anything else.
(require 'stribb-startup)
(stribb/optimize-startup)

;; Built-in modes that don't need packages.
(auto-compression-mode 1)
(editorconfig-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)

;; Straight.el bootstrap.
;; This is necessary to tell straight where git is.
(add-to-list 'exec-path "/usr/local/bin" t)
(setenv "PATH" (string-join exec-path ":"))

;; Some of the Straight variables must be set *before* the bootstrap
;; code runs.
(defvar straight-use-package-by-default)
(defvar straight-built-in-pseudo-packages)
(defvar straight-recipe-repositories)
(setq straight-use-package-by-default t)
(setq straight-built-in-pseudo-packages
      '(emacs nadvice python image-mode project flymake xref))
(setq straight-recipe-repositories
      '(org-elpa melpa gnu-elpa-mirror el-get emacsmirror-mirror))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use-package configuration.
(defvar use-package-hook-name-suffix)
(defvar use-package-compute-statistics)
(defvar flycheck-emacs-lisp-load-path)
(setq use-package-hook-name-suffix nil)
(setq use-package-compute-statistics t)
(setq flycheck-emacs-lisp-load-path 'inherit)
(eval-when-compile
  (require 'use-package)
  (require 'use-package-delight))
(require 'bind-key)

;; Restore startup vars after init completes.
(add-hook 'after-init-hook #'stribb/restore-vars :append)

;; Load config modules.
(require 'stribb-core)
(unless noninteractive
  (require 'stribb-ui)
  (require 'stribb-interactive))

;;; init.el ends here
