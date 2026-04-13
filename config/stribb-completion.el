;;; package --- stribb-completion -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Minibuffer completion framework configuration.
;;; Uses the Vertico ecosystem: vertico, consult, orderless, marginalia, embark.

;;; Code:
(require 'use-package)
(require 'cl-lib)

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
  (setq prefix-help-command #'embark-prefix-help-command)

  (setq embark-target-finders
        (cl-substitute 'stribb/embark-target-expression-in-lisp
                       'embark-target-expression-at-point
                       embark-target-finders))

  (defun stribb/embark-target-expression-in-lisp ()
    "Like `embark-target-expression-at-point', but only in lisp modes."
    (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'lisp-interaction-mode
                          'scheme-mode 'clojure-mode 'ielm-mode)
      (embark-target-expression-at-point)))

  ;; Eglot actions on identifiers when eglot is active.
  (defvar-keymap embark-eglot-map
    :doc "Actions for identifiers when eglot is active."
    "R" #'eglot-rename
    "a" #'eglot-code-actions
    "h" #'eldoc
    "r" #'xref-find-references
    "d" #'xref-find-definitions
    "i" #'eglot-find-implementation
    "t" #'eglot-find-typeDefinition)

  (push '(eglot-identifier embark-eglot-map) embark-keymap-alist)

  (defun stribb/embark-target-eglot-identifier ()
    "Target identifier at point when eglot is managing the buffer."
    (when (and (bound-and-true-p eglot--managed-mode)
               (thing-at-point 'symbol))
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (cons 'eglot-identifier
              (cons (buffer-substring-no-properties (car bounds) (cdr bounds))
                    bounds)))))

  (add-to-list 'embark-target-finders 'stribb/embark-target-eglot-identifier))

(use-package embark-consult
  :after (embark consult))

(provide 'stribb-completion)
;;; stribb-completion.el ends here
