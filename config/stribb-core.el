;;; package --- stribb-core config  -*- lexical-binding: t; -*-

;;; Commentary:
;; Make Emacs more comfortable to use.

;;; Code:

;; Speed up init.
(defvar file-name-handler-alist-old file-name-handler-alist)

(defun stribb/optimize-startup ()
  "Optimize Emacs startup performance."
  (setq
   debug-on-error t
   file-name-handler-alist nil
   message-log-max 16384
   gc-cons-threshold 4026531840 ; Start with a high threshold
   gc-cons-percentage 10.5
   auto-window-vscroll nil)
  (dolist (d '("config" "elisp-misc"))
    (add-to-list 'load-path (concat user-emacs-directory d)))
  (setq inhibit-startup-screen t)
  (add-hook 'after-init-hook #'stribb/restore-vars :append))

(defvar stribb/restart-desktop-file ".emacs-restart-desktop"
  "Desktop file that also serves as restart flag in `user-emacs-directory'.")

(defun stribb/maybe-restore-desktop ()
  "Check if this was a restart and restore desktop if so."
  (let ((desktop-file (expand-file-name stribb/restart-desktop-file user-emacs-directory)))
    (when (file-exists-p desktop-file)
      ;; Clean up the restart desktop file after it's loaded
      (add-hook 'desktop-after-read-hook
                (lambda ()
                  (let ((restart-file (expand-file-name stribb/restart-desktop-file user-emacs-directory)))
                    (when (file-exists-p restart-file)
                      (delete-file restart-file)))))
      (dlet ((desktop-base-file-name stribb/restart-desktop-file))
        (desktop-read user-emacs-directory)))))

(defun stribb/restore-vars ()
  "Restore variables and perform post-init tasks."
  (setq
   debug-on-error nil
   file-name-handler-alist file-name-handler-alist-old
   gc-cons-threshold 800000    ; Reset to a more reasonable value
   gc-cons-percentage 0.1)
  (run-with-idle-timer 10 nil #'garbage-collect)
  ;; Restore desktop after a short delay to let themes/faces settle
  (run-with-timer 0.1 nil #'stribb/maybe-restore-desktop))

(defun stribb/restart-emacs ()
  "Save desktop and restart Emacs, restoring all open buffers."
  (interactive)
  (dlet ((desktop-base-file-name stribb/restart-desktop-file))
    (desktop-save user-emacs-directory t))
  (restart-emacs))

(defalias 'rrr 'stribb/restart-emacs)

(stribb/optimize-startup)

(auto-compression-mode 1)
(editorconfig-mode 1)
(column-number-mode 1)

;; Straight.
;; This is necessary to tell `straight' where git is.
(add-to-list 'exec-path "/usr/local/bin" t)
(setenv "PATH" (string-join exec-path ":"))

;; "Some of [the Straight variables] must be set *before* the
;; bootstrap code runs..."
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



;;; General packages

;;; https://github.com/jwiegley/use-package
;;
;; :preface
;;   Timing: Before both :init and :config
;;   Usage: Define helper functions/macros needed by other parts of the
;;     declaration.
;;
;; :init
;;   Timing: Before package loading
;;   Usage: Set variables or configurations required before initialization.
;;
;; :config
;;   Timing: After package loading
;;   Usage: Activate modes, add hooks, or perform setup that depends on the
;;     package being loaded.
;;
(setq use-package-hook-name-suffix nil)
(setq use-package-compute-statistics t)

(setq flycheck-emacs-lisp-load-path 'inherit)

(eval-when-compile
  (require 'use-package)
  (require 'use-package-delight))
(require 'bind-key)

(use-package exec-path-from-shell
  :when (or (daemonp)
            (memq window-system '(mac ns)))
  :functions exec-path-from-shell-initialize
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG"
                 "LC_CTYPE" "LC_ALL" "PYENV_ROOT" "PYENV_SHELL" "PIPENV_PIPFILE"))
    (add-to-list 'exec-path-from-shell-variables var))
  ;; (when (eq system-type 'darwin)
  ;;   (setq exec-path-from-shell-check-startup-files nil))
  (exec-path-from-shell-initialize))

(use-package delight
  :functions delight
  :init
  (require 'use-package-delight)
  :config
  (delight '((emacs-lisp-mode ("Elisp" (lexical-binding ":Lex" ":Dyn")) :major)
             (eldoc-mode nil "eldoc"))))

(use-package hydra)

(use-package use-package-hydra
  :after hydra)

(use-package bash-completion
  ;; bash-completion requires Bash 4; OSX uses the last GPLv2 version.
  ;; Why can't we all get on?
  :unless (eq system-type 'darwin)
  :after eshell)

(use-package fish-completion
  :functions global-fish-completion-mode
  :after eshell
  :ensure-system-package fish
  :config
  (setq fish-completion-fallback-on-bash-p (not (eq system-type 'darwin)))
  (global-fish-completion-mode))

(use-package which-key
  :delight
  :config (which-key-mode))

(use-package bazel
  :straight (bazel
             :type git
             :host github
             :repo "bazelbuild/emacs-bazel-mode"
             :files (:defaults)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package multiple-cursors
  :functions hydra-multiple-cursors/body mc/edit-lines mc/mark-all-like-this
             mc/insert-numbers mc/insert-letters
             mc/mark-next-like-this mc/unmark-next-like-this
             mc/mark-previous-like-this mc/unmark-previous-like-this
             mc/mark-all-in-region-regexp
             mc/skip-to-next-like-this mc/skip-to-previous-like-this
             mc/add-cursor-on-click
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

(use-package num3-mode
  :straight (num3-mode :host github :repo "emacs-straight/num3-mode"
                       :fork (:host github
                                    :repo "stribb/num3-mode"))
  :delight " #"
  :functions num3-mode global-num3-mode
  :config
  (global-num3-mode t))

(use-package yasnippet
  :functions yas-global-mode
  :delight (yas-minor-mode " ✂")
  :config
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "yasnippet-go") t)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :functions company-mode global-company-mode
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

;; Apply Helm native compilation workaround unless experimental feature is enabled
(unless (stribb-feature-enabled-p 'helm-native-comp)
  (add-to-list 'native-comp-jit-compilation-deny-list "helm"))
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
         ("C-c i" . helm-imenu)
         ("M-s o" . helm-occur)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-r" . helm-recentf)
         ("C-h a" . helm-apropos)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)))

(use-package eshell
  :functions eshell-cmpl-initialize
  :init
  (defun stribb/helm-eshell-completions nil
    (eshell-cmpl-initialize)
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
  :config
  (add-hook 'eshell-mode-hook #'stribb/helm-eshell-completions))

(use-package helm-rg  ;; ripgrep
  :if (executable-find "rg")
  :after helm)

(use-package direnv
  :if (executable-find "direnv")
  :functions direnv-mode direnv-update-environment
  :config
  (direnv-mode))

(use-package eshell
  :straight nil
  :hook (eshell-post-command-hook . #'eshell-get-path))

(require 'stribb-org)

;; https://github.com/alphapapa/unpackaged.el#hydra
(use-package smerge-mode
  :functions alphapapa/smerge-hydra/body
             smerge-auto-leave smerge-next smerge-prev
             smerge-keep-base smerge-keep-upper smerge-keep-lower smerge-keep-all smerge-keep-current
             smerge-diff-base-upper smerge-diff-upper-lower smerge-diff-base-lower
             smerge-refine smerge-ediff smerge-combine-with-next smerge-resolve smerge-kill-current
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
  :functions magit-status magit-git-command-topdir magit-add-section-hook
             magit-insert-unpushed-to-upstream magit-insert-unpushed-to-pushremote
             magit-insert-recent-commits magit-insert-unpushed-to-upstream-or-recent
             transient-insert-suffix transient-append-suffix magit-push-implicitly
             magit-toplevel
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
  (when (featurep 'direnv)
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
  :functions global-diff-hl-mode diff-hl-flydiff-mode diff-hl-magit-post-refresh
  :after magit
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :functions projectile-project-name projectile-next-project-buffer
             projectile-previous-project-buffer projectile-switch-project
             projectile-find-file projectile-mode
  :bind-keymap
  (("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map))
  :bind
  (("C-`" . projectile-next-project-buffer)
   ("C-~" . projectile-previous-project-buffer))
  (:map projectile-command-map
              ("C-p" . projectile-switch-project)
              ("g" . helm-projectile-rg)
              ("p" . projectile-switch-project))
  :demand t
  :init
  (defun stribb/magit-status-or-dired ()
    (interactive)
    (if (magit-toplevel)
        (magit-status)
      (projectile-find-file)))

  (setq projectile-project-search-path
        (seq-filter #'file-directory-p '("~/experimental" "~/go/src"))
        projectile-switch-project-action 'stribb/magit-status-or-dired)
  :config
  (projectile-mode 1))

(use-package helm-projectile
  :functions helm-projectile-on helm-projectile-rg helm-projectile-switch-project
  :bind
  (:map projectile-command-map
        ("g" . helm-projectile-rg)
        ("p" . helm-projectile-switch-project)
        ("C-p" . helm-projectile-switch-project))
  :custom
  (helm-projectile-ignore-strategy 'search-tool)
  :config
  (helm-projectile-on)
  (set-face-attribute 'helm-grep-file nil :foreground "#657b83" :underline t)
  (set-face-attribute 'helm-rg-file-match-face nil :foreground "#657b83" :weight 'light))



(defun stribb/read-file (filename &optional notrim? noexpand?)
  "Read FILENAME into a string.

If NOTRIM? refrain from trimming the contents.
If NOEXPAND? don't expand the file name."
  (with-temp-buffer
    (insert-file-contents (funcall (if noexpand? 'identity 'expand-file-name) filename))
    (funcall (if notrim? 'identity 'string-trim)
             (buffer-string))))

(defun stribb/gptel-newline-or-original ()
    "Execute the original <return> command from the markdown-mode buffer."
    (interactive)
    (let ((original-command (keymap-lookup markdown-mode-map (kbd "<return>"))))
      (if original-command
          (call-interactively original-command)
        ;; Fallback to newline if original binding isn't found
        (newline))))

(defun stribb/gptel-unwrap-gemini-markdown (beg end)
  "Remove markdown backquotes that Gemini returns, from BEG to END."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (when (looking-at "^```$")
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char beg)
    (when (looking-at "^```.*$")
      (delete-region (match-beginning 0) (match-end 0))
      (delete-char 1))))

(use-package treesit-auto
  :if (treesit-available-p)
  :demand t
  :functions treesit-auto-add-to-auto-mode-alist
  :config
  (setq treesit-auto-install t)
  (treesit-auto-add-to-auto-mode-alist))

;; The track-changes package is needed for eglot.
;(use-package track-changes
;    :straight (track-changes :type git :host github :repo "emacs-straight/track-changes"))  ;; WHY??!

(use-package eglot
  :demand t
  :functions eglot-alternatives eglot-format-buffer eglot-ensure
             eglot-code-action-quickfix eglot-rename
  :bind ( :map eglot-mode-map
          ("s-<return>" . eglot-code-action-quickfix)
          ("M-r" . eglot-rename))
  :hook
  ((python-mode-hook python-ts-mode-hook) . eglot-ensure)
  ((go-mode-hook go-ts-mode-hook) . eglot-ensure)
  :custom-face
  (eglot-code-action-suggestion-face ((t (:bold t :underline t))))
  (eglot-code-action-indicator-face ((t (:bold t :underline t))))
  :config
  ;; Use flymake for LSP diagnostics, disable flycheck in LSP-managed buffers
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when flycheck-mode
                (flycheck-mode -1))
              (flymake-mode 1)))

  (setq eglot-workspace-configuration
        '((:gopls . ((gofumpt . t)))
          ;; Basedpyright configuration
          (:basedpyright . (:analysis (:typeCheckingMode "standard"
                                       :autoSearchPaths t
                                       :useLibraryCodeForTypes t
                                       :diagnosticMode "openFilesOnly")))))

  ;; Use basedpyright for Python (fallback to pyright, then pylsp)
  (add-to-list 'eglot-server-programs
               `((python-ts-mode python-mode) .
                 ,(eglot-alternatives '(("basedpyright-langserver" "--stdio")
                                       ("pyright-langserver" "--stdio")
                                       ("pylsp")))))
  (when (eq window-system 'ns)
    (define-key key-translation-map (kbd "s-<mouse-1>") (kbd "<mouse-2>"))))

(use-package subword
  :straight nil
  :hook
  ((prog-mode-hook markdown-mode-hook yaml-mode-hook) . (lambda () (subword-mode t))))

(use-package jq-mode
  :mode "\\.jq\\'"
  :interpreter "jq")

(use-package restclient
  :interpreter "restclient")

;; It's like a general purpose paredit-mode.
(use-package smartparens
  :functions sp-wrap-round sp-wrap-square sp-wrap-curly
             sp-wrap-with-pair sp-use-smartparens-bindings
             show-smartparens-global-mode turn-on-smartparens-mode
             turn-on-smartparens-strict-mode
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

(use-package ws-butler
  :delight
  :functions ws-butler-global-mode
  :config (ws-butler-global-mode))

(use-package flycheck
  :demand t
  :functions global-flycheck-mode flycheck-mode flymake-mode
  :init
  (global-flycheck-mode 1)

  (defun stribb/ensure-flycheck-over-flymake ()
  "Ensure `flymake-mode' is disabled if `flycheck-mode' is active."
  ;; flycheck-mode should be active due to (global-flycheck-mode 1)
  (when (and (boundp 'flycheck-mode) flycheck-mode)
    (flymake-mode -1)))

  (add-hook 'prog-mode-hook #'stribb/ensure-flycheck-over-flymake))

(use-package elisp-format
  :commands (elisp-format-file elisp-format-region elisp-format-buffer)
  :config (setq-default elisp-format-column 80))

(use-package eldoc
  :delight
  :config (global-eldoc-mode))

(use-package buttercup)

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
    :functions irony-cdb-autosetup-compile-options irony-eldoc
               irony-completion-at-point-async
    :config
    (irony-cdb-autosetup-compile-options)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (add-hook 'c++-mode-hook 'irony-eldoc)
    :bind (:map irony-mode-map
                ([remap completion-at-point] . irony-completion-at-point-async)
                ([remap complete-symbol] . irony-completion-at-point-async)))

  (use-package flycheck-irony
    :after irony
    :functions flycheck-irony-setup
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
    (add-to-list 'company-backends 'company-irony))

  (use-package platformio-mode
    :functions platformio-conditionally-enable
    :config
    (add-hook 'c++-mode-hook 'platformio-conditionally-enable)))

(use-package json-mode
  :mode ("\\.jso?n\\'" . json-mode))

(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("\\.crd\\'" . yaml-mode))
  :magic ("^# vi: set ft=yaml" . yaml-mode)
  :bind (:map yaml-mode-map ("C-m" . newline-and-indent)))

(use-package ansible  ; a minor mode
  :after yaml-mode
  :functions ansible-mode
  :init
  (defun stribb-yaml--enable-ansible-if-needed ()
    "Activate `ansible-mode` and `eglot` for Ansible project files."
    ;; Use `when-let` to make the check cleaner and avoid errors if
    ;; buffer-file-name is nil (for non-file buffers).
    (when-let* ((filename (buffer-file-name))
                ((string-match-p (rx "/ansible/" (0+ anything) "/tasks/") filename)))
      (ansible-mode 1)
      (eglot-ensure)))
    :hook (yaml-mode . stribb-yaml--enable-ansible-if-needed))

(use-package puppet-mode
  :mode "\\.pp\\'")

(use-package terraform-mode
  :mode "\\.tf\\'"
  :functions terraform-format-on-save-mode
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; (when nil
;;   (use-package haskell-mode
;;     :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
;;            ("\\.lhs\\'" . literate-haskell-mode)
;;            ("\\.cabal\\'" . haskell-cabal-mode)))

;;   (use-package intero
;;     :after haskell-mode
;;     :hook (haskell-mode-hook . intero-mode)))

(use-package go-mode
  :mode "\\.go\\'\\|/go\\.mod"
  :config
  (setq gofmt-command "goimports")
  (defun stribb/go-mode ()
    (when (local-variable-p 'fill-column)
      (setq fill-column 95))
    (when (local-variable-p 'go-ts-mode-indent-offset)
      (setq go-ts-mode-indent-offset 4)))
  :hook ((go-ts-mode-hook . stribb/go-mode)
         (go-mode-hook . stribb/go-mode)))

(use-package asdf
  :straight (asdf :type git :host github :repo "tabfugnic/asdf.el")
  :config (asdf-enable))

(use-package erlang
  :mode ("\\.erl\\'" . erlang-mode))

(use-package distel
  :straight (distel :type git :host github :repo "massemanet/distel"
		    :pre-build nil :files ("elisp/*.el"))
  :after erlang
  :config
  (distel-setup))

(use-package company-distel
  :after distel)

(use-package flycheck-tip
  :after erlang-mode)

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

(require 'stribb-python)

(use-package groovy-mode
  :mode "\\.groovy\\'")


;;; Non-package config.
(defmacro prog0 (&rest body)
  "Evaluate BODY forms in turn, returning null."
  `(prog1 nil ,@body))

(defun stribb/java-indentation ()
  "Set the Java indentation to stribb's favourite style."
  (interactive)
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'stribb/java-indentation)

(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil)
  (require 'ls-lisp))


(defun stribb/delete-char-hungry ()
  "Delete char forward, joining lines and deleting indentation at EOL.

Acts like `delete-char'.  However, when deleting a newline (i.e.,
when called at the end of a line), this command also deletes any
subsequent horizontal whitespace."
  (interactive)
  (let ((was-at-eol (eolp)))
    ;; Use the original delete-char command for the primary action.
    (call-interactively #'delete-char)
    ;; If we just deleted a newline, clean up the following indentation.
    (when was-at-eol
      ;; Find the end of the whitespace streak after point.
      (delete-char (save-excursion (skip-chars-forward "[:blank:]"))))))

;; TODO: use smartparens
(defun old-forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point.

With ARG, go ARG forward or backward."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun previous-error-or-scroll-up ()
  "If Flycheck errors in buffer, go to previous one.  Otherwise scroll up."
  (interactive)
  (call-interactively (if flycheck-current-errors
                         'previous-error
                        'scroll-up-command)))

(defun next-error-or-scroll-down ()
  "If Flycheck errors in buffer, go to next one.  Otherwise scroll down."
  (interactive)
  (call-interactively (if flycheck-current-errors
                          'next-error
                        'scroll-down-command)))

(defun stribb/open-init-file (n)
  "Opens the init file.  With argument N: if 4, open in other window; 5: frame."
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
         (filename (if (provided-mode-derived-p mode non-file-modes)
                       default-directory
                     (buffer-file-name buffer))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(defalias 'bfn 'prelude-copy-file-name-to-clipboard)


(defmacro assert (test-form)
  "Asserts that TEST-FORM evaluate to non-nil."
  `(unless ,test-form
     (error "Assertion failed: %s" (format "%s" ',test-form))))

(defun stribb/transpose-windows ()
  "Switch this window for the next window in the same frame."
  (interactive)
  (when-let* ((other-window (next-window))
	      (this-window (selected-window))
	      (new-selected-buffer (window-buffer other-window))
	      (new-other-buffer (window-buffer this-window)))
    (assert (not (eq this-window other-window)))
    (set-window-buffer this-window new-selected-buffer)
    (set-window-buffer other-window new-other-buffer)
    (select-window other-window)))

(defun stribb/forward-up-to-char (arg char)
  "Move forward to the ARGth occurrence of CHAR."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char "Go to char: ")))
  (search-forward (char-to-string char) nil nil arg)
  (forward-char -1))

(defun stribb/just-save-buffer ()
  "Mask out save hooks and save the buffer."
  (interactive)
  (let (before-save-hook
        write-contents-functions
        after-save-hook)
    (save-buffer)))

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(bind-keys  ;; Sorted by function name.
 ("<home>" . beginning-of-buffer)
 ("C-x B" . bury-buffer)
 ("M-SPC" . cycle-spacing)
 ("<f1> <f1>" . display-local-help)
 ("<end>" . end-of-buffer)
 ("C-q" . old-forward-or-backward-sexp)
 ("M-Z" . stribb/forward-up-to-char)
 ("C-x l" . (lambda () (interactive)
              (message "`C-x l' is obsolete! Use s-l or M-g g instead!")))
 ("C-M-r" . isearch-backward)
 ("C-r" . isearch-backward-regexp)
 ("C-M-s" . isearch-forward)
 ("C-s" . isearch-forward-regexp)
 ("s-f" . isearch-forward-regexp)
 ("<kp-add>" . next-error)
 ("<next>" . next-error-or-scroll-down)
 ("s-<down>" . next-error-or-scroll-down)
 ("<kp-subtract>" . previous-error)
 ("<prior>" . previous-error-or-scroll-up)
 ("s-<up>" . previous-error-or-scroll-up)
 ("M-%" . query-replace-regexp)
 ("C-S-q" . quoted-insert)
 ("s-s" . save-buffer)
 ("C-<prior>" . scroll-down-command)
 ("C-<next>" . scroll-up-command)
 ("C-d" .  stribb/delete-char-hungry)
 ("s-," . stribb/open-init-file)
 ("H-," . stribb/open-init-file)
 ("C-x 4 t" . stribb/transpose-windows)
 ("s-0" . text-scale-adjust)
 ("s--" . text-scale-adjust)
 ("s-=" . text-scale-adjust)
 ("C-z" . undo)
 ("M-/" . xref-find-references)
 ("M-<kp-add>" . xref-find-definitions)
 ("M-<kp-subtract>" . xref-go-back)
 ("M-z" . zap-up-to-char))

(use-package sh-script
  :straight nil
  :preface
  (defun stribb/server-edit-done ()
    "Tell the client we're done with the buffer, then kill it."
    (interactive)
    (save-buffer)
    (server-edit)
    (kill-buffer-and-window))
  :bind (:map sh-mode-map
	      ("C-x #" . stribb/server-edit-done)))

(use-package eshell
  :straight nil
  :init
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

(use-package simple
  :straight nil
  :custom
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  :config
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode))

(use-package isearch
  :straight nil
  :bind
  (:map isearch-mode-map
   ("<up>" . isearch-ring-retreat)
   ("<down>" . isearch-ring-advance)
   ("<left>" . isearch-repeat-backward)
   ("<right>" . isearch-repeat-forward))
  (:map minibuffer-local-isearch-map
   ("<left>" . isearch-reverse-exit-minibuffer)
   ("<right>" . isearch-forward-exit-minibuffer))
  :init
  (defun stribb/isearch-region (&optional not-regexp no-recursive-edit)
    "If a region is active, make this the isearch default search pattern.

    Arguments NOT-REGEXP and NO-RECURSIVE-EDIT mirror the isearch function
    args."
    (interactive "P\np")
    (when (use-region-p)
      (let ((search (buffer-substring-no-properties
                     (region-beginning)
                     (region-end))))
        (setq deactivate-mark t)
        (isearch-yank-string search))))
  :config
  (dolist (f '(isearch-forward-regexp
               isearch-forward
               isearch-backward-regexp
               isearch-backward))
    (advice-add f :after 'stribb/isearch-region)))

(use-package help-mode
  :straight nil
  :init
  (defun stribb/help-mode-revert-buffer (&rest _)
    "Unconditionally revert `help-mode' buffer."
    (interactive)
    (help-mode-revert-buffer nil t))
  :hook
  (help-mode-hook . (lambda ()
                      (setq revert-buffer-function #'stribb/help-mode-revert-buffer))))

(midnight-mode)
(midnight-delay-set 'midnight-delay "03:00")

(use-package kubernetes
  :functions kubernetes-overview
  :config
  (defalias 'k8s #'kubernetes-overview))

;; Disable because we're not using GPG as an SSH agent everywhere.
(when nil
  (unless (string-prefix-p "/tmp/ssh-" (getenv "SSH_AUTH_SOCK"))
    (setenv "SSH_AUTH_SOCK" (format "/run/user/%d/gnupg/S.gpg-agent.ssh" (user-uid)))))

(setq
 apropos-do-all t
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 disabled-command-function nil
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

;; Bash and zsh fc:
(add-to-list 'auto-mode-alist '("/bash-fc|/tmp/zsh" . sh-mode))

(message "Core finished: %d GCs, startup time %s" gcs-done (emacs-init-time))

(provide 'stribb-core)
;;; stribb-core.el ends here
