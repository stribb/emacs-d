;;; package --- stribb-completion -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Minibuffer completion framework configuration.
;;; Uses the Vertico ecosystem: vertico, consult, orderless, marginalia, embark.

;;; Code:
(require 'use-package)

;; Vertico: Modern vertical completion UI
(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("<escape>" . minibuffer-keyboard-quit))
  :config
  (vertico-mode))

;; Orderless: Flexible completion style
(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil
        ;; Enable initialism matching: "cn" matches "crows-nest"
        orderless-matching-styles '(orderless-initialism
                                    orderless-literal
                                    orderless-regexp)))

;; Marginalia: Rich annotations in the minibuffer
(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;; Consult: Useful search and navigation commands
(use-package consult
  :functions consult-register-format consult-register-window
             consult-xref consult--read
  :bind (;; Core commands
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-x r b" . consult-bookmark)

         ;; Search and navigation
         ("C-c i" . consult-imenu)
         ("M-s o" . consult-line)
         ("M-s g" . consult-ripgrep)
         ("M-s d" . consult-find)
         ("M-g g" . consult-goto-line)

         ;; Help commands
         ("C-h a" . consult-apropos))
  :config
  (setq consult-narrow-key "<"
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Embark: Contextual actions
(use-package embark
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

(provide 'stribb-completion)
;;; stribb-completion.el ends here
