;;; package --- stribb-ui -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; This file contains all visual and UI-related settings. This includes:
;;; - Theme loading
;;; - Font configuration (face attributes)
;;; - Modeline setup (e.g., doom-modeline)
;;; - Disabling window chrome (scrollbars, toolbars, menubar)

;;; Code:
(require 'use-package)

(use-package solarized-theme
  :config
  (setq custom-enabled-themes '(solarized-dark solarized-light)
        solarized-high-contrast-mode-line t)
  (load-theme 'solarized-light t))

(use-package doom-modeline
  :demand t
  :functions (doom-modeline-def-segment doom-modeline-def-modeline doom-modeline-spc)
  :commands (doom-modeline-mode)
  :config
  (doom-modeline-mode 1)

  ;; Hide buffer encoding (UTF-8, LF, etc.) - only show when unusual
  (setq doom-modeline-buffer-encoding nil)

  ;; Add auto-revert indicator to mode-line-misc-info
  (setq-default mode-line-misc-info
                (append mode-line-misc-info
                        '((:eval
                           (when (and (boundp 'auto-revert-mode) auto-revert-mode)
                             (propertize " ↻"
                                         'face '(:inherit success :weight bold)
                                         'help-echo "Auto-revert is ON\nmouse-1: Toggle auto-revert"
                                         'mouse-face 'mode-line-highlight
                                         'local-map (let ((map (make-sparse-keymap)))
                                                      (define-key map [mode-line mouse-1]
                                                        (lambda ()
                                                          (interactive)
                                                          (auto-revert-mode 'toggle)))
                                                      map))))))))

;; Thanks to https://amitp.blogspot.com/2014/04/emacs-rainbow-identifiers.html
(use-package rainbow-identifiers
  :hook (prog-mode-hook . rainbow-identifiers-mode)
  :config
  (add-to-list 'rainbow-identifiers-faces-to-override
               'font-lock-function-name-face)
  (set-face-attribute 'font-lock-variable-name-face nil :foreground 'unspecified)
  (setq-default rainbow-identifiers-choose-face-function
                'rainbow-identifiers-cie-l*a*b*-choose-face
                rainbow-identifiers-cie-l*a*b*-lightness 45
                rainbow-identifiers-cie-l*a*b*-saturation 40
                rainbow-identifiers-cie-l*a*b*-color-count 15))

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package ligature
  :demand t
  :commands (global-ligature-mode)
  :config
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   ;; Copied from https://www.jetbrains.com/lp/mono/#ligatures
   (split-string "-- --- == === != !== =!= =:= =/= <= >= && &&& &=
 ++ +++ *** ;; !! ?? ??? ?: ?. ?= <: :< :> >: <:< <> <<< >>> <<
 >> || -| _|_ |- ||- |= ||= ## ### #### #{ #[ ]# #( #? #_ #_( #:
 #! #= ^= <$> <$ $> <+> <+ +> <*> <* *> </ </> /> <!-- <#-- -->
 -> ->> <<- <- <=< =<< <<= <== <=> <==> ==> => =>> >=> >>= >>- >-
 -< -<< >-> <-< <-| <=| |=> |-> <-> <~~ <~ <~> ~~ ~~> ~> ~- -~ ~@ [||]
 |] [| |} {| [< >] |> <| ||> <|| |||> <||| <|> ... .. .= ..< .?
 :: ::: := ::= :? :?> // /// /* */ /= //= /== @_ __ ??? <:< ;;;"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(setq-default default-frame-alist
              (append '((tool-bar-lines . 0))
                      (when (display-graphic-p)
                        '((alpha-background . 100)
                          (height . 70)
                          (width . 103)))))

(use-package whitespace
  :config
  (setq whitespace-style '(face lines-tail)
        whitespace-line-column nil)
  :hook (prog-mode-hook . whitespace-mode))

(use-package highlight-indentation
  :hook
  (yaml-mode . highlight-indentation-mode))

(add-hook 'after-init-hook
          (lambda ()
            (set-frame-font "JetBrainsMono Nerd Font-13" nil t)
            (set-fontset-font t 'unicode "JetBrainsMono Nerd Font" nil 'prepend)))


;; Line numbers in the gutter
(setq-default display-line-numbers nil)
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers t)))

(use-package ansi-color
  ;; Compile mode deserves some ANSI colour love
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Non-local Emacs files should start read-only
(dir-locals-set-class-variables
 'readonly
 '((nil . ((eval . (when buffer-file-name
                     (setq-local view-no-disable-on-exit t)
                     (view-mode-enter)))))))
(let ((dirs '("/usr/local/src/emacs" "/usr/local/share/emacs"
             "/usr/share/emacs" "~/.emacs.d/elpa/"
             "/Applications/Emacs.app/Contents/Resources/lisp/")))
  (dolist (d dirs)
    (dir-locals-set-directory-class (expand-file-name d) 'readonly)))

(provide 'stribb-ui)
;;; stribb-ui.el ends here
