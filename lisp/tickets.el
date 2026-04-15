;;; tickets --- follow a file containing a list of tickets.  -*- lexical-binding: t -*-

;;; Commentary:

;;  Create a whitespace-separated file of tickets.  Then:
;;    (tickets-file "/path/to/tickets/file")
;;  Now tickets-list will by dynamically updated with the tickets.

;;; Code:
(require 'filenotify)

(defvar tickets-filename nil
  "The file that is watched for tickets.")

(defvar tickets-watcher nil
  "The ticket watcher.")

(defvar tickets-list nil
  "Which tickets exist in the watched file.")

(defun tickets-file (filename)
  "Watch FILENAME for tickets."
  (interactive "f")
  (let* ((fullname (expand-file-name filename))
	 (shortname (abbreviate-file-name fullname)))
    (message "Watching %s for tickets" shortname)
    (when (and tickets-filename
	       (not (string-equal tickets-filename fullname)))
      (tickets--unwatch))
    (setq tickets-watcher (tickets--watch fullname))
    (setq tickets-filename fullname)))

(defun tickets--watch (filename)
  "Start watching FILENAME."
  (let ((cb #'tickets--change-callback))
    (file-notify-add-watch filename '(change) cb)
    (funcall cb (list nil nil filename))))

(defun tickets--unwatch nil
  "Finish watching FILENAME."
  (file-notify-rm-watch tickets-watcher))

(defun tickets--change-callback (cb)
  "Process filenotify callback CB."
  (let ((filename (caddr cb)))
    (with-temp-buffer
      (insert-file-contents filename)
      (setq tickets-list (split-string (buffer-string))))))


(provide 'tickets)
;;; tickets.el ends here
