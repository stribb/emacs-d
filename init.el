;;; package --- loader for the stribb configs -*- lexical-binding: t; -*-
;;; Commentary:
;;; This file loads the modular components of my personal Emacs setup.

;;; Code:

;; Add the configuration directory to the load-path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Load the core configuration, identified by its unique feature symbol
(require 'stribb-core)

;; Conditionally load interactive-only settings
(unless noninteractive
  (require 'stribb-ui)
  (require 'stribb-interactive))

;;; init.el ends here
