;;; package --- stribb-vcs -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Version control configuration: smerge, magit, diff-hl, projectile.

;;; Code:

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
  (defvar-local stribb/gerrit-target-branch nil
    "Branch to push to on Gerrit. Auto-detected from git remote. Override per-project via .dir-locals.el.")
  (put 'stribb/gerrit-target-branch 'safe-local-variable #'stringp)
  (defun stribb/get-gerrit-target-branch ()
    "Get target branch, auto-detecting from git if not set."
    (or stribb/gerrit-target-branch
        (string-remove-prefix
         "origin/"
         (string-trim (shell-command-to-string "git-default-branch")))))
  (defun magit-push-to-gerrit ()
    (interactive)
    (magit-git-command-topdir
     (format "git push origin HEAD:refs/for/%s" (stribb/get-gerrit-target-branch))))
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
        ("g" . consult-ripgrep)
        ("p" . projectile-switch-project))
  :demand t
  :init
  (defun stribb/magit-status-or-dired ()
    (interactive)
    (cond
     ((ignore-errors (magit-toplevel)) (magit-status))
     ((eq (projectile-project-vcs) 'jj) (dired (projectile-project-root)))
     (t (projectile-find-file))))

  (defvar stribb/projectile-native-cache (make-hash-table :test 'equal)
    "Separate cache for native mode indexing (all files including gitignored).")

  (defun stribb/projectile-find-file-with-all (orig-fun &rest args)
    "Advice to show all files (including gitignored) when called with prefix arg.
With C-u, temporarily use native indexing to show gitignored files.
Maintains separate caches for alien and native modes."
    (if current-prefix-arg
        (let ((projectile-indexing-method 'native)
              ;; Swap to native cache
              (projectile-projects-cache stribb/projectile-native-cache))
          (prog1 (apply orig-fun args)
            ;; Save native cache back
            (setq stribb/projectile-native-cache projectile-projects-cache)))
      (apply orig-fun args)))

  (setq projectile-project-search-path
        (seq-filter #'file-directory-p '("~/experimental" "~/go/src"))
        projectile-switch-project-action 'stribb/magit-status-or-dired
        ;; Disable git submodule scanning - submodule--helper binary bypasses ~/bin/git wrapper
        projectile-git-submodule-command "")
  :config
  (projectile-mode 1)
  (advice-add 'projectile-find-file :around #'stribb/projectile-find-file-with-all))

(provide 'stribb-vcs)
;;; stribb-vcs.el ends here
