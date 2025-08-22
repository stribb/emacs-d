
;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)

;;; Experimental Features System
;; Feature flags for testing modern alternatives and removing legacy workarounds
(defvar stribb-experimental-features
  '(;; Remove Helm native compilation workaround
    ;; Set to t if Helm works fine with native compilation in your setup
    (helm-native-comp . nil))
  "Feature flags for experimental configuration changes.")

(defun stribb-feature-enabled-p (feature)
  "Check if FEATURE is enabled in experimental features."
  (cdr (assq feature stribb-experimental-features)))

;;; Old fogey mode.
(message "Disabling fogeyphobic UI elements...")
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
