;;; package --- Summary  tools for gptel to use -*- lexical-binding: t -*-

;;; Commentary:
;; https://github.com/karthink/gptel?tab=readme-ov-file#tool-use

;;; Code:

(require 'gptel)

(gptel-make-tool
 :name "read_buffer"
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Buffer %s is not live" buffer))
             (with-current-buffer buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
               :type string            ; :type value must be a symbol
               :description "the name of the buffer whose contents are to be retrieved"))
 :category "emacs")


(gptel-make-tool
 :name "list_directory"
 :function (fset 'gptel--tool-list-directory
                 (lambda (directory)
                   "List the contents of DIRECTORY."
                   (if (file-directory-p directory)
                       (mapcar (lambda (f) (cons f (file-attributes f 'string)))
                               (directory-files directory))
                     ;; Return an error indicator if the path is not a directory
                     (error "Not a directory: %s" directory))))
 :description "List the contents of a directory as elisp s-expression
 `(filename . file-attributes)'."
 :args (list '(:name "directory"
               :type string
               :description "The path to the directory to list."))
 :category "emacs")

(require 'gptel-git-context)

(add-to-list 'gptel-tools :gemini/code-execution)

;; Grounding is incompatible with functions:
;; https://ai.google.dev/gemini-api/docs/grounding?lang=rest .
;;(add-to-list 'gptel-tools :gemini/google-search)


(provide 'gptel-tools)
;;; gptel-tools.el ends here
