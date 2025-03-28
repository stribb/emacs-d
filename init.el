;;; package --- stribb .emacs file

;;; Commentary:
;; Make Emacs more comfortable to use.

;;; Code:

;; Speed up init.
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq debug-on-error t
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)
(add-hook 'after-init-hook
          #'(lambda ()
              (setq debug-on-error nil
                    file-name-handler-alist file-name-handler-alist-old
                    gc-cons-threshold 800000
                    gc-cons-percentage 0.1)
              (garbage-collect)) t)

(dolist (d '("config" "elisp-misc"))
  (add-to-list 'load-path (concat user-emacs-directory d)))

(setq inhibit-startup-screen t)

;;; Straight.

;; This is necessary to tell `straight' where git is.
(add-to-list 'exec-path "/usr/local/bin" t)
(setenv "PATH" (string-join exec-path ":"))

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


;;; General packages

;;; https://github.com/jwiegley/use-package

(use-package use-package
  :config
  (setq use-package-hook-name-suffix nil
        use-package-compute-statistics t))

(setq flycheck-emacs-lisp-load-path 'inherit)
(require 'straight)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package straight
  :custom
  ;; https://github.com/radian-software/straight.el/issues/1146
  (straight-use-package-by-default t)
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode flymake xref)))

(use-package exec-path-from-shell
  :config
  (when (string= system-type "darwin")
      (setq exec-path-from-shell-check-startup-files nil))
  (exec-path-from-shell-initialize))

(use-package hydra)

(use-package use-package-hydra
  :after hydra)

(use-package bash-completion
  :after eshell)

(use-package fish-completion
  :after eshell bash-completion
  :ensure-system-package fish
  :config
  (setq fish-completion-fallback-on-bash-p t)
  (global-fish-completion-mode))

(use-package doom-modeline
  :config (doom-modeline-mode 1))

(use-package bazel
  :straight (bazel
             :type git
             :host github
             :repo "bazelbuild/emacs-bazel-mode"
             :files (:defaults)))

(use-package delight
  :config
  (delight '((emacs-lisp-mode ("Elisp" (lexical-binding ":Lex" ":Dyn")) :major)
             (eldoc-mode nil "eldoc"))))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package multiple-cursors
  :hydra (hydra-multiple-cursors
          nil
          "Multiple cursors"
          ("l" mc/edit-lines "edit" :exit t :column "Instant")
          ("a" mc/mark-all-like-this "mark all" :exit t)
          ("0" mc/insert-numbers "ins 0.." :exit t)
          ("A" mc/insert-letters "ins A..":exit t)
          ("q" nil "quit")
          ("n" mc/mark-next-like-this "next" :column "Mark")
          ("M-n" mc/unmark-next-like-this "-next")
          ("p" mc/mark-previous-like-this "prev")
          ("M-p" mc/unmark-previous-like-this "-prev")
          ("r" mc/mark-all-in-region-regexp "regex" :exit t)
          ("N" mc/skip-to-next-like-this "next" :column "Skip/Insert")
          ("P" mc/skip-to-previous-like-this "prev")
          ("<mouse-1>" mc/add-cursor-on-click "click")
          ;; Help with click recognition in this hydra
          ("<down-mouse-1>" ignore :hide t)
          ("<drag-mouse-1>" ignore :hide t))
  :bind
  ("C-c M" . hydra-multiple-cursors/body)
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-=" . mc/mark-all-like-this))

(use-package which-key
  :delight
  :config (which-key-mode))

(use-package num3-mode
  :straight (num3-mode :host github :repo "emacs-straight/num3-mode"
                       :fork (:host github
                                    :repo "stribb/num3-mode"))
  :delight " #"
  :config
  (global-num3-mode t))

(use-package yasnippet
  :delight (yas-minor-mode " ✂")
  :config
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "yasnippet-go") t)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :bind (("<C-tab>" . company-complete))
  :init (setq completion-styles '(basic partial-completion emacs22 initials))
  :delight " Co"
  :config
  (company-mode)
  (global-company-mode))

(use-package async
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(use-package popup)

;; C-x c is the default key binding for helm-command-prefix-key.
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

(use-package helm-rg  ;; ripgrep
  :if (executable-find "rg")
  :after helm)

(use-package direnv
  :if (executable-find "direnv")
  :preface
  (defun stribb/eshell-env-to-path ()
    "Put $PATH into `eshell-env-path'"
    (setq eshell-path-env (getenv "PATH")))
  :config
  (direnv-mode)
  (add-hook 'eshell-post-command-hook #'stribb/eshell-env-to-path))

(use-package org
  :init (load "org-config")
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ;; ("C-c C-," . org-insert-structure-template)  <-- already default
         ("C-c C-x TAB" . org-clock-in)))

(use-package htmlize
  :after org)

(use-package org-pomodoro
  :after org
  :bind (("C-c C-x p" . org-pomodoro)
         ("C-c C-x C-p" . org-pomodoro)))

(use-package solarized-theme
  :config
  (setq custom-enabled-themes '(solarized-dark solarized-light)
        solarized-high-contrast-mode-line t)
  (load-theme 'solarized-light t))

;; https://github.com/alphapapa/unpackaged.el#hydra
(use-package smerge-mode
  :bind (:map smerge-mode-map
              ("C-c h" . alphapapa/smerge-hydra/body))
  :after hydra
  :config
  (defhydra alphapapa/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine               _ZZ_: save and bury
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file-hook . (lambda ()
                                        (when smerge-mode
                                          (alphapapa/smerge-hydra/body)))))

(use-package magit
  ;; Work around https://github.com/dgutov/diff-hl/issues/65 -- let's see if it works again?
  ;; :init (global-auto-revert-mode)
  :bind (("C-x g" . magit-status)
	 :map magit-mode-line-process-map
	 ("C-<return>" . helm-find-files))

  :demand t
  :config
  (defun magit-push-to-gerrit ()
    (interactive)
    (magit-git-command-topdir "git push origin HEAD:refs/for/master"))

  (setq magit-log-section-commit-count 20)
  (define-key with-editor-mode-map "\C-cP" 'git-commit-prev-message)
  (if (executable-find "direnv")
      (add-hook 'magit-mode-hook #'direnv-update-environment)) ; ?
  (add-to-list 'magit-section-initial-visibility-alist '(recent . show))
  (add-to-list 'magit-section-initial-visibility-alist '(unstaged . show))
  (transient-insert-suffix 'magit-push "o"
    '("i" magit-push-implicitly))
  (transient-append-suffix 'magit-push "-n"
    '("-c" "GH: Create merge request"
      "--push-option=merge_request.create"))
  (transient-append-suffix 'magit-push "-c"
    '("-m" "GH: Merge on success"
      "--push-option=merge_request.merge_when_pipeline_succeeds"))
  (transient-append-suffix 'magit-push "-m"
    '("-r" "GH: Remove source branch"
      "--push-option=merge_request.remove_source_branch"))
  (transient-append-suffix 'magit-push "-r"
    '("-w" "Gerrit: Mark WIP" "--push-option=wip"))
  (transient-append-suffix 'magit-push "-w"
    '("-W" "Gerrit: Mark Ready (remove WIP)" "--push-option=ready"))
  (transient-append-suffix 'magit-fetch "-t"
    '("-f" "Bypass safety checks" "--force"))
  (transient-append-suffix 'magit-push "m"
    '("G" "Push to gerrit" magit-push-to-gerrit))
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-pushremote)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-recent-commits
                          'magit-insert-unpushed-to-upstream)
  (remove-hook 'magit-status-sections-hook
               'magit-insert-unpushed-to-upstream-or-recent)
  (add-hook 'magit-mode (lambda () (num3-mode nil))))

(use-package diff-hl
  :after magit
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind (("C-`" . projectile-next-project-buffer)
         ("C-~" . projectile-previous-project-buffer))
  :demand t
  :init
  (defun stribb/magit-status-or-dired ()
    (interactive)
    (if (magit-toplevel)
        (magit-status)
      (projectile-find-file)))

  (setq projectile-project-search-path
        (seq-filter #'file-directory-p '("~/experimental" "~/go/src"))
        ;; (projectile-discover-projects-in-search-path)
        projectile-switch-project-action 'stribb/magit-status-or-dired)
  :config
  (run-with-idle-timer 20 3600 'projectile-discover-projects-in-search-path)
  (projectile-mode))

(use-package helm-projectile
  :after (helm projectile)
  :bind (:map projectile-command-map
              ("C-p" . projectile-switch-project)
              ("g" . helm-projectile-rg)
              ("p" . projectile-switch-project))
  :bind-keymap
  (("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map))
  :custom
  (helm-projectile-ignore-strategy 'search-tool)
  :config
  (helm-projectile-on)
  (set-face-attribute 'helm-grep-file nil :foreground "#657b83" :underline t)
  (set-face-attribute 'helm-rg-file-match-face nil :foreground "#657b83" :weight 'light))

(use-package projectile-ripgrep)



;;; Programming modes

;; The track-changes package is needed for eglot.
(use-package track-changes
    :straight (track-changes :type git :host github :repo "emacs-straight/track-changes"))  ;; WHY??!

(use-package eglot
  :bind ("s-<return>" . eglot-code-action-quickfix)
  :config
  (setq eglot-workspace-configuration
        '((:gopls . ((gofumpt . t))))))

;; It's like a general purpose paredit-mode.
(use-package smartparens
  :hook
  ((prog-mode-hook markdown-mode-hook yaml-mode-hook) . turn-on-smartparens-mode)
  ((ielm-mode-hook elisp-mode-hook) . turn-on-smartparens-strict-mode)
  :bind (:map smartparens-mode-map
              ("M-(" . sp-wrap-round)
              ("M-[" . sp-wrap-square)
              ("M-{" . sp-wrap-curly)
              ("M-'" . stribb/wrap-single)
              ("M-\"" . stribb/wrap-double))
  :config
  (defun stribb/wrap-single ()
    "Wrap following sexp in single quotes."
    (interactive)
    (sp-wrap-with-pair "'"))
  (defun stribb/wrap-double ()
    "Wrap following sexp in double quotes."
    (interactive)
    (sp-wrap-with-pair "\""))
  (show-paren-mode nil)
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode t)
  (require 'smartparens-config))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; Thanks to https://amitp.blogspot.com/2014/04/emacs-rainbow-identifiers.html
(use-package rainbow-identifiers
  :hook (prog-mode-hook . rainbow-identifiers-mode)
  :config
  (add-to-list 'rainbow-identifiers-faces-to-override
               'font-lock-function-name-face)
  (set-face-attribute 'font-lock-variable-name-face nil :foreground nil)
  (setq-default rainbow-identifiers-choose-face-function
                'rainbow-identifiers-cie-l*a*b*-choose-face
                rainbow-identifiers-cie-l*a*b*-lightness 45
                rainbow-identifiers-cie-l*a*b*-saturation 40
                rainbow-identifiers-cie-l*a*b*-color-count 15))

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package ws-butler
  :delight
  :config (ws-butler-global-mode))

;; (defun flycheck-virtualenv-setup (&rest args) nil)
(use-package flycheck
  :config
  (require 'flycheck-virtualenv)
  ;; (add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup)
  (global-flycheck-mode))

(use-package elisp-format
  :commands (elisp-format-file elisp-format-region elisp-format-buffer)
  :config (setq-default elisp-format-column 80))

(use-package eldoc
  :delight
  ;; :hook
  ;; ((emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook) . turn-on-eldoc-mode)
  :config (global-eldoc-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

(progn
  (use-package irony
    :hook (c++-mode-hook c-mode-hook objc-mode-hook)
    :after company
    :config
    (irony-cdb-autosetup-compile-options)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (add-hook 'c++-mode-hook 'irony-eldoc)
    :bind (:map irony-mode-map
                ([remap completion-at-point] . irony-completion-at-point-async)
                ([remap complete-symbol] . irony-completion-at-point-async)))

  (use-package flycheck-irony
    :after irony
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
    (add-to-list 'company-backends 'company-irony))

  (use-package platformio-mode
    :config
    (add-hook 'c++-mode-hook 'platformio-conditionally-enable)))

(use-package json-mode
  :mode ("\\.jso?n\\'" . json-mode))

(use-package yaml-mode
  :mode ("\\.ya?ml\\|\\.crd\\'" . yaml-mode)
  :bind (:map yaml-mode-map ("C-m" . newline-and-indent))
  :config
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
  (add-to-list 'magic-mode-alist '("^# vi: set ft=yaml" . yaml-mode)))

(use-package ansible
  :preface
  (defun yaml-probably-ansible-p ()
    "Guess whether the current buffer is likely to be Ansible code."
    (string-match-p (rx (and "/ansible/" (0+ anything) "/tasks/"))
                    buffer-file-name))
  :config
  (add-hook 'yaml-mode-hook
            #'(lambda ()
                (when (yaml-probably-ansible-p)
                  (ansible-mode t)))))

(use-package puppet-mode
  :mode "\\.pp\\'")

(use-package terraform-mode
  :mode "\\.tf\\'"
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(when nil
  (use-package haskell-mode
    :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
           ("\\.lhs\\'" . literate-haskell-mode)
           ("\\.cabal\\'" . haskell-cabal-mode)))

  (use-package intero
    :after haskell-mode
    :hook (haskell-mode-hook . intero-mode)))

(progn  ;; golang
  (use-package go-mode
    :mode "\\.go\\'\\|/go\\.mod"
    :config
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    :hook (go-mode-hook . (lambda () (setq fill-column 95))))

  (use-package go-eldoc
    :after go-mode
    :config (add-hook 'go-mode-hook 'go-eldoc-setup)))

(use-package asdf
  :straight (asdf :type git :host github :repo "tabfugnic/asdf.el")
  :config (asdf-enable))

(progn
  (use-package erlang
    ; :mode ("\\.erl\\'" . erlang-mode)
    :config
    (require 'erlang-start))

  (use-package distel
    :disabled
    :straight (distel :type git :host github :repo "massemanet/distel")
    :load-path "straight/build/distel/elisp"
    :after erlang
    :config
    (require 'distel)
    (distel-setup))

  (use-package company-distel
    :after distel)

  (use-package flycheck-tip
    :after erlang-mode))

(progn
  (use-package elixir-mode
    :mode "\\.exs\\'"
    :config
    (add-hook 'elixir-mode-hook
              (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

  (use-package alchemist
    :after elixir-mode))

(progn
  (use-package gleam-ts-mode
    :straight (gleam-ts-mode
               :type git
               :host github
               :repo "gleam-lang/gleam-mode")))

(progn
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
    ;; flycheck > flymake
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)
          elpy-rpc-python-command "/home/stribb/.pyenv/shims/python")
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (use-package blacken
    :after python
    :config (add-hook 'elpy-mode-hook 'blacken-mode)))


(use-package groovy-mode
  :mode "\\.groovy\\'")


(when nil
  (use-package clojure-mode
    :mode "\\.clj\\'"
    :after smartparens
    :config
    (add-hook 'clojure-mode-hook #'turn-on-smartparens-strict-mode)
    (add-hook 'clojure-mode-hook #'turn-on-eldoc-mode))

  (use-package clojure-mode-extra-font-locking
    :after clojure-mode)

  (use-package clj-refactor
    :disabled
    :after clojure-mode
    :config
    (defun clj-refactor-clj-mode-hook nil
      (clj-refactor-mode 1)
      (yas-minor-mode 1) ; for adding require/use/import statements
      ;; This choice of keybinding leaves cider-macroexpand-1 unbound
      (cljr-add-keybindings-with-prefix "C-c C-m"))
    (add-hook 'clojure-mode-hook #'clj-refactor-clj-mode-hook))

  (use-package cider
    :after clojure-mode
    :config
    (add-hook 'cider-repl-mode-hook #'turn-on-smartparens-strict-mode)
    (setq-default cider-save-file-on-load t
                  cider-prompt-for-symbol nil
                  cider-eval-result-prefix ";; => "))

  (use-package slamhound
    :disabled
    :after cider
    :config
    (defun stribb/add-slamhound-to-before-save-hook nil
      (add-hook 'before-save-hook #'slamhound nil t))
    (add-hook 'clojure-mode-hook #'stribb/add-slamhound-to-before-save-hook)))



;;; Non-package config.

;;; Old fogey mode.
(progn
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1)))

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Line numbers
(setq-default display-line-numbers nil)
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers t)))

(defmacro prog0 (&rest body)
  "Evaluate BODY forms in turn, returning null."
  `(prog1 nil ,@body))

(defun stribb/java-indentation ()
  "Set the Java indentation to stribb's favourite style."
  (interactive)
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'stribb/java-indentation)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil)
  (require 'ls-lisp))


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

;; TODO: use smartparens
(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point.
With ARG, go ARG forward or backward."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun previous-error-or-scroll-down ()
  "If Flycheck errors in buffer, go to previous one.  Otherwise scroll down."
  (interactive)
  (call-interactively (if flycheck-current-errors
                         'previous-error
                        'scroll-down-command)))

(defun next-error-or-scroll-up ()
  "If Flycheck errors in buffer, go to previous one.  Otherwise scroll up."
  (interactive)
  (call-interactively (if flycheck-current-errors
                          'next-error
                        'scroll-up-command)))

(defun stribb/open-init-file (n)
  "Opens the init file."
  (interactive "p")
  (let ((buf (find-file-noselect user-init-file)))
    (cond
     ((= n 4)
      (switch-to-buffer-other-window buf))
     ((= n 5)
      (switch-to-buffer-other-frame buf))
     (t
      (switch-to-buffer buf)))))

(defun current-frame-move-to-monitor (n)
  "Move the current frame to monitor N."
  (interactive "p")
  (set-frame-position (selected-frame)
                      (+ 50 (* n 1800))
                      50))

(defvar non-file-modes '(eshell-mode dired-mode magit-mode)
  "List of directory-located non-file-modes.")

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let* ((buffer (or (buffer-base-buffer) (current-buffer)))
         (mode (with-current-buffer buffer major-mode))
         (filename (if (apply #'provided-mode-derived-p mode non-file-modes)
                       default-directory
                     (buffer-file-name buffer))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defalias 'bfn 'prelude-copy-file-name-to-clipboard)
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(defun stribb/server-edit-done ()
  "Tell the client we're done with the buffer, then kill it."
  (interactive)
  (save-buffer)
  (server-edit)
  (kill-buffer-and-window))

(bind-keys  ;; Sorted by function name.
 ("<home>" . beginning-of-buffer)
 ("C-x B" . bury-buffer)
 ("M-SPC" . cycle-spacing)
 ("<f1> <f1>" . display-local-help)
 ("<end>" . end-of-buffer)
 ("C-q" . forward-or-backward-sexp)
 ("M-Z" . forward-up-to-char)
 ("C-x l" . goto-line)
 ("C-M-r" . isearch-backward)
 ("C-r" . isearch-backward-regexp)
 ("C-M-s" . isearch-forward)
 ("C-s" . isearch-forward-regexp)
 ("s-f" . isearch-forward-regexp)
 ("<kp-add>" . next-error)
 ("<next>" . next-error-or-scroll-up)
 ("s-<down>" . next-error-or-scroll-up)
 ("M-/" . pop-tag-mark)
 ("<kp-subtract>" . previous-error)
 ("<prior>" . previous-error-or-scroll-down)
 ("s-<up>" . previous-error-or-scroll-down)
 ("M-%" . query-replace-regexp)
 ("C-S-q" . quoted-insert)
 ("s-s" . save-buffer)
 ("C-<prior>" . scroll-down-command)
 ("C-<next>" . scroll-up-command)
 ("s-," . stribb/open-init-file)
 ("H-," . stribb/open-init-file)
 ("s-0" . text-scale-adjust)
 ("s--" . text-scale-adjust)
 ("s-=" . text-scale-adjust)
 ("C-z" . undo)
 ("M-/" . xref-find-references)
 ("M-<kp-add>" . xref-find-definitions)
 ("M-<kp-subtract>" . xref-go-back)
 ("M-z" . zap-up-to-char))


(use-package sh-script
  :bind (:map sh-mode-map
	      ("C-x #" . stribb/server-edit-done)))

(use-package eshell
  :preface
  (defun stribb/eshell-prompt-function ()
    "Custom prompt function."
    (let ((ok? eshell-last-command-status))
      (concat (with-face ": " :background (if ok? "gray" "red"))
              (abbreviate-file-name (eshell/pwd))
              (if (= (user-uid) 0) " # " " ; "))))
  :config
  (setq eshell-prompt-regexp "^: [^#$;\n]* [#$;] "
	eshell-prompt-function #'stribb/eshell-prompt-function
	eshell-hist-ignoredups t))

(progn  ;; visual-line mode
  (setq visual-line-fringe-indicators '(nil right-curly-arrow))
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode))

(progn  ;; isearch gubbins
  ;; http://ergoemacs.org/emacs/emacs_isearch_by_arrow_keys.html set
  ;; arrow keys in isearch. left/right is backward/forward, up/down is
  ;; history.
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

  (define-key minibuffer-local-isearch-map (kbd "<left>")
    'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>")
    'isearch-forward-exit-minibuffer))

(defun stribb/isearch-region (&optional not-regexp no-recursive-edit)
  "If a region is active, make this the isearch default search pattern.

Arguments NOT-REGEXP and NO-RECURSIVE-EDIT mirror the isearch function args."
  (interactive "P\np")
  (when (use-region-p)
    (let ((search (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))))
      (setq deactivate-mark t)
      (isearch-yank-string search))))

(dolist (f '(isearch-forward-regexp
             isearch-forward
             isearch-backward-regexp
             isearch-backward))
  (advice-add f :after 'stribb/isearch-region))

(progn  ;; help-mode hijinks
  (defun stribb/help-mode-revert-buffer (ignore-auto noconfirm)
    "Unconditionally revert `help-mode' buffer."
    (interactive)
    (help-mode-revert-buffer nil t))

  (defun stribb/help-mode-setup ()
    "Set up `help-mode' my way."
    (setq revert-buffer-function #'stribb/help-mode-revert-buffer))

  (add-hook 'help-mode-hook #'stribb/help-mode-setup))

(defun transpose-windows ()
  "Switch this window for the other window in the same frame."
  (interactive)
  (let ((this-buffer (window-buffer (selected-window)))
        (other-buffer (prog2
                          (other-window +1)
                          (window-buffer (selected-window))
                        (other-window -1))))
    (switch-to-buffer other-buffer)
    (switch-to-buffer-other-window this-buffer)
    (other-window -1)))
(define-key ctl-x-4-map (kbd "t") 'transpose-windows)

(defun forward-up-to-char (arg char)
  "Move forward to the ARGth occurence of CHAR."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Go to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (search-forward (char-to-string char) nil nil arg)
  (left-char))

(defun stribb/just-save-buffer ()
  "Mask out save hooks and save the buffer."
  (interactive)
  (let (before-save-hook
        write-contents-functions
        after-save-hook)
    (save-buffer)))

(defun vc-rename-visited-file (new)
  "Rename the file this buffer is visiting to NEW."
  (interactive (list (read-file-name "Rename to: ")))
  (when (not (buffer-file-name))
    (error "This buffer is not visiting a file"))
  (vc-rename-file (buffer-file-name) new))

(midnight-mode)
(midnight-delay-set 'midnight-delay "03:00")

(setq-default default-frame-alist
              (append '((tool-bar-lines . 0))
                      (when (display-graphic-p)
                        '((alpha-background . 100)
                          (height . 70)
                          (width . 103)))))

(use-package whitespace
  :config
  (setq whitespace-style '(face lines-tail))
  :hook (prog-mode-hook . whitespace-mode))

(set-frame-font "JetBrainsMono Nerd Font-11" nil t)
(set-fontset-font t 'unicode "JetBrainsMono Nerd Font" nil 'prepend)

(use-package kubernetes
  :config
  (fset 'k8s 'kubernetes-overview))

(use-package ligature
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

;; Disable because we're not using GPG as an SSH agent everywhere.
(when nil
  (unless (string-prefix-p "/tmp/ssh-" (getenv "SSH_AUTH_SOCK"))
    (setenv "SSH_AUTH_SOCK" (format "/run/user/%d/gnupg/S.gpg-agent.ssh" (user-uid)))))

(setq-default
 apropos-do-all t
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 ediff-window-setup-function 'ediff-setup-windows-default
 indent-tabs-mode nil
 load-prefer-newer t
 mouse-yank-at-point t
 require-final-newline t
 save-interprogram-paste-before-kill t
 save-place-file (concat user-emacs-directory "places")
 sh-learn-basic-offset t
 smerge-command-prefix "\C-cm"
 text-scale-mode-step 1.1
 vc-follow-symlinks t
 view-read-only t
 visible-bell t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(defadvice en/disable-command (around put-in-custom-file activate)
  "Put novice.el declarations in `custom-file'."
  (let ((user-init-file custom-file))
    ad-do-it))

;; Bash and zsh fc:
(add-to-list 'auto-mode-alist '("/bash-fc|/tmp/zsh" . sh-mode))

(if (or (not (boundp 'server-process))
        (not server-process))
    (server-start))

(message "Init finished: %d GCs, startup time %s" gcs-done (emacs-init-time))

;;; init.el ends here
