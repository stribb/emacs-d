;;; gptel-git-context.el --- GPTEl tool to get context from git repo -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A tool to provide N documents of context in response to a search
;; query for a given repo.

(require 'cl-lib) ; For cl-delete-duplicates, plist-get
(require 'subr-x) ; For string-empty-p (though (zerop (length ...)) also works)
(require 'gptel)

;;; Code:

;;----------------------------------------------------------------------------
;; Customizable Variables
;;----------------------------------------------------------------------------

(defgroup gptel-git-context nil
  "Settings for the gptel git repository context tool."
  :group 'gptel-tools)

(defcustom gptel-context-git-preferred-tool (if (executable-find "rg") "rg" "git-grep")
  "Preferred command for searching in Git repositories.
If \"rg\" is chosen but not found, it will not automatically fall back."
  :type '(choice (const :tag "ripgrep (rg)" "rg")
                 (const :tag "git grep" "git-grep"))
  :group 'gptel-git-context)

(defcustom gptel-context-git-max-files 5
  "Maximum number of files to return as context."
  :type 'integer
  :group 'gptel-git-context)

(defcustom gptel-context-git-rg-extra-args
  '("--glob=!*.{log,lock,swp,bak,tmp,o,obj,elc,pyc,map,min.js,min.css}"
    "--glob=!.git/"
    "--glob=!node_modules/"
    "--glob=!target/"
    "--glob=!build/"
    "--glob=!dist/"
    "--glob=!vendor/")
  "Extra arguments for ripgrep (rg) when used by the git context tool.
Each string is a separate argument.  Useful for adding more custom ignores.
Default globs aim to exclude common build artifacts, logs, and large dependency
directories."
  :type '(repeat string)
  :group 'gptel-git-context)

;;----------------------------------------------------------------------------
;; Internal Helper Functions
;;----------------------------------------------------------------------------
(defun gptel-context--build-command (search-tool project-root-abs keyword-quoted)
  "Build the command list for rg or git-grep.

SEARCH-TOL is either the string \"git-grep\" or \"rg\".
PROJECT-ROOT-ABS is the absolute path to the project root.
KEYWORD-QUOTED is the shell-quoted keyword."
  (cond
   ((string= search-tool "rg")
    (unless (executable-find "rg")
      (signal 'gptel-tool-error
              '(:message "ripgrep (rg) executable not found in PATH.")))
    (append '("rg" "--color=never" "--files-with-matches" "--max-count" "1"
              "--no-messages" "-L")
            gptel-context-git-rg-extra-args
            (list keyword-quoted "."))) ; "." means current dir (which will be project-root-abs)

   ((string= search-tool "git-grep")
    (unless (executable-find "git")
      (signal 'gptel-tool-error (list :message "git executable not found in PATH.")))
    (unless (file-directory-p project-root-abs)
      (signal 'gptel-tool-error (list :message (format "Repository path '%s' is not a directory." project-root-abs))))
    (let ((git-check-status (call-process "git" nil nil nil "-C" (shell-quote-argument project-root-abs) "rev-parse" "--is-inside-work-tree")))
      (unless (zerop git-check-status)
        (signal 'gptel-tool-error (list :message (format "'%s' is not a git repository or git command failed during check." project-root-abs)))))
    (list "git" "-C" (shell-quote-argument project-root-abs) "grep" "--color=never" "-l" "-I"
          "--max-count=1"
          ;; Search working tree (tracked & untracked respecting .gitignore) and staging area
          "--cached"       ; Search the staging area (index)
          "--untracked"    ; Search untracked files (respects .git/info/exclude, .gitignore)
          "--exclude-standard" ; Use .gitignore and global gitignore
          keyword-quoted))

   (t (signal 'gptel-tool-error (list :message (format "Invalid search tool: '%s'. Check `gptel-context-git-preferred-tool`." search-tool))))))

(defun gptel-context--execute-search (command-list)
  "Execute the search COMMAND-LIST and return list of files.
Assumes `default-directory' is set to PROJECT-ROOT-ABS if needed by the command
\(e.g., for rg searching '.')."
  (let ((exit-status 0)
        (output-buffer (generate-new-buffer "*gptel-grep-output*"))
        (error-buffer (generate-new-buffer "*gptel-grep-error*"))
        output-string)
    (unwind-protect
        (progn
          ;; `call-process` args: program &rest-of-command-list... in-source out-destination err-destination &rest args
          (setq exit-status (apply #'call-process (car command-list) nil `(,output-buffer ,error-buffer) nil (cdr command-list)))
          (setq output-string (with-current-buffer output-buffer (buffer-string)))
          (unless (zerop exit-status)
            (signal 'gptel-tool-error
                    (list :message (format "Search command failed (status %s): %s\nError: %s"
                                           exit-status
                                           (string-join command-list " ")
                                           (with-current-buffer error-buffer (buffer-string)))))))
      (kill-buffer output-buffer)
      (kill-buffer error-buffer))
    output-string))

;;----------------------------------------------------------------------------
;; GPTEl Tool Definition
;;----------------------------------------------------------------------------
(defun gptel-tool-read-git-repository-context--function (args)
  "Implementation function for the gptel tool to read git repo context.
ARGS is a plist with :repo and :keyword."
  (let* ((repo-path (plist-get args :repo))
         (keyword (plist-get args :keyword))
         (_ (unless (and repo-path (not (string-empty-p repo-path)))
              (signal 'gptel-tool-error (list :message "Repository path cannot be empty."))))
         (_ (unless (and keyword (not (string-empty-p keyword)))
              (signal 'gptel-tool-error (list :message "Keyword cannot be empty."))))
         (project-root-abs (file-name-as-directory (expand-file-name repo-path)))
         (quoted-keyword (shell-quote-argument keyword))
         command-list
         file-list-str
         (original-default-directory default-directory))

    (unless (file-directory-p project-root-abs)
      (signal 'gptel-tool-error
              (list :message
                    (format "Repository path '%s' is not a valid directory." repo-path))))

    (setq command-list (gptel-context--build-command
                        gptel-context-git-preferred-tool
                        project-root-abs
                        quoted-keyword))

    ;; Set default-directory for commands like `rg ... .` that expect to run in the project root.
    (setq default-directory project-root-abs)
    (unwind-protect
        (setq file-list-str (gptel-context--execute-search command-list))
      (setq default-directory original-default-directory)) ; Restore original directory

    (let* ((files (cl-delete-duplicates (delete "" (split-string file-list-str "\n"))
                                        :test #'string=))
           (context-strings '())
           (files-processed 0))

      (unless files
        (signal 'gptel-tool-error (list :message (format "No files found matching '%s' in '%s' using %s."
                                                         keyword repo-path gptel-context-git-preferred-tool))))

      (dolist (relative-file files)
        (when (< files-processed gptel-context-git-max-files)
          (let ((full-file-path (expand-file-name relative-file project-root-abs)))
            (if (and (file-exists-p full-file-path) (file-readable-p full-file-path))
                (condition-case err
                    (let ((file-content (with-temp-buffer
                                          (insert-file-contents-literally full-file-path)
                                          (buffer-string))))
                      (push (format "--- Content from: %s ---\n%s\n--- End of %s ---\n"
                                    relative-file ; Show relative path for brevity
                                    file-content
                                    relative-file)
                            context-strings)
                      (setq files-processed (1+ files-processed)))
                  (error (message "gptel-git-context: Error reading file %s: %s" full-file-path err)
                         ;; You could signal 'gptel-tool-error here if a single file read error should stop everything
                         ;; For now, just messages and skips the file.
                         ))
              (message "gptel-git-context: File '%s' (from grep result '%s') is not readable or does not exist." full-file-path relative-file)))))

      (if (null context-strings)
          (signal 'gptel-tool-error (list :message (format "No readable files found or content extracted for keyword '%s' in '%s'." keyword repo-path)))
        (string-trim (string-join (nreverse context-strings) "\n"))))))


(gptel-make-tool
 :name "grep_git_repo"
 :function #'gptel-tool-read-git-repository-context--function
 :description
 "Searches a local Git repository for a keyword and returns the content of top matching files.
Uses `ripgrep` (rg) if available and configured, otherwise `git grep`.
The search targets the working tree, staged files, and untracked files (respecting .gitignore).
Configure behavior with `gptel-context-git-*` custom variables."
 :args (list '(:name "repo"
                     :type string
                     :description "Path to the local Git repository root.")
             '(:name "keyword"
                     :type string
                     :description "Keyword to search for")))

(provide 'gptel-git-context)

;;; gptel-git-context.el ends here
