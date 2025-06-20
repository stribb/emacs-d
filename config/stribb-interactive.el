;;; package --- stribb-interactive -*- lexical-binding: t; -*-

;;; Commentary:
;;; This file contains interactive-only settings, including:
;;; - Desktop loading
;;; - Server starting

;;; Code:
(desktop-save-mode 1)

(use-package server
  :straight nil
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'stribb-interactive)
;;; stribb-interactive.el ends here
