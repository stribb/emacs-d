;;; package --- PagerDuty client -*- lexical-binding: t -*-

;;; Commentary:
;; A client library for interacting with the PagerDuty API.
;; Reads the API token from ~/.pagerduty-token.
;; Adds functionality to list open incidents and acknowledge them interactively.

;;; Code:

(require 'json)
(require 'url)

;; --- Configuration ---

(defconst pagerduty-api--token-file "~/.pagerduty-token"
  "The path to the file containing the PagerDuty API token.")

(defconst pagerduty-api--base-url "https://api.pagerduty.com"
  "The base URL for the PagerDuty API.")

(defvar pagerduty-api--token-buffer nil
  "The buffer holding the PagerDuty API token.")

(defvar pagerduty-incidents-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'pagerduty-acknowledge-incident)
    map)
  "Keymap for `pagerduty-incidents-mode'.")

(define-minor-mode pagerduty-incidents-mode
  "Minor mode for interacting with PagerDuty incidents in a buffer."
  :init-value nil
  :lighter " PagerDuty"
  :keymap pagerduty-incidents-mode-map
  (if pagerduty-incidents-mode
      (message "PagerDuty incidents mode enabled.")
    (message "PagerDuty incidents mode disabled.")))

;; --- API Token Handling ---

(defun pagerduty-api--read-token ()
  "Store the PagerDuty API token in `pagerduty-api--token-buffer'.
   Read it from `pagerduty-api--token-file'.
   Returns the token string."
  (unless (file-readable-p pagerduty-api--token-file)
    (error "PagerDuty API token file not found or not readable: %s"
           pagerduty-api--token-file))
  (unless (buffer-live-p pagerduty-api--token-buffer)
    (setq pagerduty-api--token-buffer (generate-new-buffer "*pagerduty-api-token*"))
    (with-current-buffer pagerduty-api--token-buffer
      (setq-local major-mode 'fundamental-mode)
      (setq-local visible-mode nil)))
  (with-current-buffer pagerduty-api--token-buffer
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (insert-file-contents pagerduty-api--token-file)
    ;; Remove potential trailing newline
    (save-excursion
      (replace-regexp "[\r\n]+$" "" nil (point-min) (point-max)))
    (setq-local buffer-read-only t)
    (buffer-string)))

;; --- PagerDuty API Request Function ---

(defun pagerduty-api--request (method path &optional body params)
  "Make a request to the PagerDuty API.
   METHOD is the HTTP method (e.g., 'GET, 'PUT).
   PATH is the API endpoint path (e.g., \"/incidents\").
   BODY is an optional JSON object for PUT/POST requests.
   PARAMS is an optional alist of query parameters for GET requests.
   Returns the parsed JSON response."
  (let* ((token (pagerduty-api--read-token))
         (query-string (if params (url-build-query params) "")) ; Corrected line
         (full-url (concat pagerduty-api--base-url path
                           (if (string-empty-p query-string) "" (concat "?" query-string)))) ; Corrected line
         (request-headers `(("Authorization" . ,(concat "Token token=" token))
                           ("Accept" . "application/vnd.pagerduty+json;version=2")
                           ("Content-Type" . "application/json")))
         (request-body (if body (json-encode body) nil))
         (url-request-method (intern (upcase (symbol-name method))))
         (buffer (url-retrieve-synchronously full-url
                                             url-request-method
                                             request-body
                                             request-headers)))
    (with-current-buffer buffer
      (goto-char (point-min))
      ;; Skip HTTP headers to get to the JSON body
      (re-search-forward "^$" nil t)
      (read-json))))
;; --- Incident Listing ---

(defun pagerduty-list-incidents ()
  "Fetch and display open PagerDuty incidents in a new buffer."
  (interactive)
  (let* ((incidents-buffer (get-buffer-create "*PagerDuty Incidents*"))
         (incidents (pagerduty-api--request 'GET "/incidents"
                                            nil
                                            '(("statuses[]" . "triggered")
                                              ("statuses[]" . "acknowledged")
                                              ("sort_by" . "created_at:desc")))))
    (with-current-buffer incidents-buffer
      (erase-buffer)
      (setq-local buffer-read-only nil) ; Allow modification for display
      (insert "Open PagerDuty Incidents:\n\n")
      (dolist (incident (cdr (assoc 'incidents incidents))) ; incidents is an alist, get the value of 'incidents' key
        (let* ((incident-id (cdr (assoc 'id incident)))
               (summary (cdr (assoc 'summary incident)))
               (status (cdr (assoc 'status incident)))
               (created-at (cdr (assoc 'created_at incident)))
               (service-name (cdr (assoc 'summary (cdr (assoc 'service incident)))))
               (assigned-to (mapconcat (lambda (assignment)
                                         (cdr (assoc 'summary (cdr (assoc 'assignee assignment)))))
                                       (cdr (assoc 'assignments incident))
                                       ", ")))
          (insert (format "ID: %s\n" incident-id))
          (insert (format "  Summary: %s\n" summary))
          (insert (format "  Status: %s\n" status))
          (insert (format "  Service: %s\n" service-name))
          (insert (format "  Created: %s\n" created-at))
          (insert (format "  Assigned To: %s\n" (if (string-empty-p assigned-to) "Unassigned" assigned-to)))
          (insert "\n")
          ;; Add text property to the entire line for easy lookup
          (put-text-property (point-at-bol) (point-at-eol) 'pagerduty-incident-id incident-id)))

      (insert "\nPress 'a' to acknowledge the incident at point.\n")
      (setq-local buffer-read-only t) ; Make buffer read-only again
      (pagerduty-incidents-mode 1) ; Enable the minor mode
      (goto-char (point-min))
      (recenter 0)
      (display-buffer incidents-buffer))))

;; --- Incident Acknowledgment ---

(defun pagerduty-get-incident-id-at-point ()
  "Get the PagerDuty incident ID at the current cursor position.
   Looks for the `pagerduty-incident-id' text property on the current line."
  (get-text-property (point) 'pagerduty-incident-id))

(defun pagerduty-acknowledge-incident (incident-id)
  "Acknowledge PagerDuty incident INCIDENT-ID.

  If an incident ID is found at point, acknowledge that incident.
  Otherwise, prompt the user for an incident ID to acknowledge."
  (interactive (list (let ((id (pagerduty-get-incident-id-at-point)))
                       (if id id (read-string "Acknowledge incident ID: ")))))
  (if incident-id
      (if (y-or-n-p (format "Acknowledge incident %s? " incident-id))
          (progn
            (message "Acknowledging incident %s..." incident-id)
            (let* ((response (pagerduty-api--request 'PUT
                                                     (format "/incidents/%s" incident-id)
                                                     `((incident . ((type . "incident_reference")
                                                                    (status . "acknowledged")))))))
              (if (cdr (assoc 'incident response))
                  (message "Incident %s acknowledged successfully." incident-id)
                (error "Failed to acknowledge incident %s: %S" incident-id response))
              (pagerduty-list-incidents))) ; Refresh the list
        (message "Acknowledgment cancelled."))
    (message "No PagerDuty incident ID provided.")))

(provide 'pagerduty)
;;; pagerduty.el ends here
