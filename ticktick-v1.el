;;; ticktick-v1.el --- TickTick V1 (OAuth2) backend -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Paul Huang
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (simple-httpd "1.5.0"))
;; Keywords: tools, ticktick, oauth, v1
;; URL: https://github.com/polhuang/ticktick.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file implements the TickTick V1 (OAuth2) backend.
;; It provides the official OAuth2-based API access with long-lived tokens.
;;
;; Features:
;; - OAuth2 authorization code flow
;; - Automatic token refresh
;; - Local callback server for authorization
;; - Stable, official API

;;; Code:

(require 'ticktick-backend)
(require 'ticktick-common)
(require 'request)
(require 'json)
(require 'url)
(require 'simple-httpd)
(require 'cl-lib)

;;; Customization Variables --------------------------------------------------

(defcustom ticktick-v1-client-id ""
  "TickTick V1 OAuth client ID.
Obtain this from https://developer.ticktick.com/"
  :type 'string
  :group 'ticktick)

(defcustom ticktick-v1-client-secret ""
  "TickTick V1 OAuth client secret.
Obtain this from https://developer.ticktick.com/"
  :type 'string
  :group 'ticktick)

(defcustom ticktick-v1-auth-scopes "tasks:write tasks:read"
  "Space-separated OAuth scopes for TickTick V1 API access."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-v1-redirect-uri "http://localhost:8080/ticktick-callback"
  "OAuth redirect URI for V1 authorization.
Must match the URI registered in your TickTick OAuth app settings."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-v1-httpd-port 8080
  "Local port for the OAuth callback server."
  :type 'integer
  :group 'ticktick)

(defcustom ticktick-v1-token-file
  (expand-file-name ".ticktick-v1-token"
                    (concat user-emacs-directory "ticktick/"))
  "File in which to store TickTick V1 OAuth token."
  :type 'file
  :group 'ticktick)

;;; Backend Structure --------------------------------------------------------

(cl-defstruct (ticktick-v1-backend
               (:constructor ticktick-v1-backend-create)
               (:copier nil))
  "V1 backend structure."
  (token nil)
  (oauth-state nil))

;;; Token Persistence --------------------------------------------------------

(defun ticktick-v1--ensure-dir ()
  "Ensure token directory exists."
  (let ((dir (file-name-directory ticktick-v1-token-file)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun ticktick-v1--save-token (backend)
  "Save BACKEND token to file."
  (when-let ((token (ticktick-v1-backend-token backend)))
    (ticktick-v1--ensure-dir)
    (with-temp-file ticktick-v1-token-file
      (insert (prin1-to-string token)))))

(defun ticktick-v1--load-token (backend)
  "Load token from file into BACKEND."
  (when (file-exists-p ticktick-v1-token-file)
    (with-temp-buffer
      (insert-file-contents ticktick-v1-token-file)
      (goto-char (point-min))
      (setf (ticktick-v1-backend-token backend) (read (current-buffer))))))

;;; OAuth2 Functions ---------------------------------------------------------

(defun ticktick-v1--authorization-header ()
  "Create basic authentication header for TickTick V1 API."
  (concat "Basic "
          (base64-encode-string
           (concat ticktick-v1-client-id ":" ticktick-v1-client-secret) t)))

(defun ticktick-v1--make-token-request (form-params)
  "Make a token request with FORM-PARAMS and return the response data."
  (let* ((form-data
          (mapconcat
           (lambda (kv)
             (format "%s=%s"
                     (url-hexify-string (car kv))
                     (url-hexify-string (format "%s" (cdr kv)))))
           form-params
           "&"))
         (authorization (ticktick-v1--authorization-header))
         (response-data nil))
    (request "https://ticktick.com/oauth/token"
      :type "POST"
      :headers `(("Authorization" . ,authorization)
                 ("Content-Type" . "application/x-www-form-urlencoded"))
      :data form-data
      :parser (lambda ()
                (let ((json-object-type 'plist)
                      (json-array-type 'list))
                  (json-read)))
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq response-data data)))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (message "V1 token request failed: %s (HTTP %s)"
                         (or error-thrown "unknown error")
                         (and response (request-response-status-code response)))
                (setq response-data nil))))
    (when (and response-data (plist-get response-data :access_token))
      (setq response-data
            (plist-put response-data :created_at (float-time))))
    response-data))

(defun ticktick-v1--exchange-code-for-token (code)
  "Exchange the authorization CODE for an access token."
  (ticktick-v1--make-token-request
   `(("grant_type" . "authorization_code")
     ("code" . ,code)
     ("redirect_uri" . ,ticktick-v1-redirect-uri)
     ("scope" . ,ticktick-v1-auth-scopes))))

(defun ticktick-v1--start-callback-server ()
  "Start the local OAuth callback server if not already running."
  (setq httpd-port ticktick-v1-httpd-port)
  (unless (httpd-running-p)
    (httpd-start)))

(defservlet ticktick-callback text/plain (_path query)
            "Handle the TickTick V1 OAuth redirect."
            (let ((code  (cadr (assoc "code" query)))
                  (state (cadr (assoc "state" query)))
                  (backend ticktick--v1-backend))
              (cond
               ((not (and code state))
                (insert "Authentication failed: missing code/state."))
               ((and backend
                     (ticktick-v1-backend-oauth-state backend)
                     (not (string= state (ticktick-v1-backend-oauth-state backend))))
                (insert "Authentication failed: invalid state."))
               (t
                (let ((tok (ticktick-v1--exchange-code-for-token code)))
                  (if tok
                      (progn
                        (setf (ticktick-v1-backend-token backend) tok)
                        (ticktick-v1--save-token backend)
                        (insert "Authentication successful! You can close this window.")
                        (message "TickTick V1: authenticated successfully."))
                    (insert "Authentication failed while exchanging code.")
                    (message "TickTick V1: token exchange failed.")))))))

;;; Token Management ---------------------------------------------------------

(defun ticktick-v1-token-expired-p (token)
  "Check if TOKEN has expired."
  (let ((expires-in (plist-get token :expires_in))
        (created-at (plist-get token :created_at)))
    (if (and expires-in created-at)
        (> (float-time) (+ created-at expires-in -30))
      nil)))

(defun ticktick-v1--refresh-token-internal (backend)
  "Refresh the OAuth2 token for BACKEND."
  (when-let ((token (ticktick-v1-backend-token backend)))
    (let* ((refresh-token (plist-get token :refresh_token))
           (response-data (ticktick-v1--make-token-request
                           `(("grant_type" . "refresh_token")
                             ("refresh_token" . ,refresh-token)
                             ("redirect_uri" . ,ticktick-v1-redirect-uri)
                             ("scope" . ,ticktick-v1-auth-scopes)))))
      (if response-data
          (progn
            (setf (ticktick-v1-backend-token backend) response-data)
            (ticktick-v1--save-token backend)
            (message "V1 token refreshed!"))
        (message "Failed to refresh V1 token.")))))

(defun ticktick-v1--ensure-token (backend)
  "Ensure BACKEND has a valid access token."
  (ticktick-v1--load-token backend)
  (let ((token (ticktick-v1-backend-token backend)))
    (unless (and token
                 (plist-get token :access_token)
                 (not (ticktick-v1-token-expired-p token)))
      (ticktick-v1--refresh-token-internal backend))))

;;; API Request Functions ----------------------------------------------------

(defun ticktick-v1--parse-json-maybe ()
  "Parse current buffer as JSON if non-empty; otherwise return nil."
  (goto-char (point-min))
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false :false))
    (if (zerop (buffer-size))
        nil
      (condition-case _ (json-read)
        (json-error nil)))))

(defun ticktick-v1-request (backend method endpoint &optional data)
  "Send a V1 API request using BACKEND.
METHOD is the HTTP method, ENDPOINT is the API path, DATA is optional body."
  (ticktick-v1--ensure-token backend)
  (let* ((url (concat "https://api.ticktick.com" endpoint))
         (token (ticktick-v1-backend-token backend))
         (access-token (plist-get token :access_token))
         (headers `(("Authorization" . ,(concat "Bearer " access-token))
                    ("Content-Type" . "application/json")))
         (json-data (and data (json-encode data)))
         (response-data nil))
    (request url
      :type method
      :headers headers
      :data json-data
      :parser #'ticktick-v1--parse-json-maybe
      :sync t
      :success (cl-function
                (lambda (&key data response &allow-other-keys)
                  (let ((status (request-response-status-code response)))
                    (cond
                     ((and (>= status 200) (< status 300))
                      (setq response-data data))
                     ((= status 401)
                      (ticktick-v1--refresh-token-internal backend)
                      (setq response-data (ticktick-v1-request backend method endpoint data)))
                     (t (error "HTTP Error %s" status))))))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (let ((status (and response (request-response-status-code response))))
                  (cond
                   ((= status 401)
                    (ticktick-v1--refresh-token-internal backend)
                    (setq response-data (ticktick-v1-request backend method endpoint data)))
                   (t
                    (message "V1 request failed: %s"
                             (or (and response (request-response-data response))
                                 error-thrown))
                    (setq response-data nil)))))))
    response-data))

;;; V1 <-> Internal Conversion -----------------------------------------------

(defun ticktick-v1--task-to-internal (v1-task)
  "Convert V1 API task plist to internal task structure."
  (ticktick-task-create
   :id (plist-get v1-task :id)
   :title (plist-get v1-task :title)
   :status (if (= (plist-get v1-task :status) 2)
               ticktick-status-completed
             ticktick-status-active)
   :priority (or (plist-get v1-task :priority) 0)
   :due-date (plist-get v1-task :dueDate)
   :content (plist-get v1-task :content)
   :etag (plist-get v1-task :etag)
   :project-id (plist-get v1-task :projectId)
   :tags (plist-get v1-task :tags)
   :kind "TEXT"
   :created-time (plist-get v1-task :createdTime)
   :modified-time (plist-get v1-task :modifiedTime)))

(defun ticktick-v1--internal-to-api-task (task)
  "Convert internal TASK structure to V1 API format (alist)."
  (let ((alist '()))
    (when-let ((id (ticktick-task-id task)))
      (push (cons "id" id) alist))
    (when-let ((title (ticktick-task-title task)))
      (push (cons "title" title) alist))
    (push (cons "status"
                (if (eq (ticktick-task-status task) ticktick-status-completed)
                    2 0))
          alist)
    (when-let ((priority (ticktick-task-priority task)))
      (push (cons "priority" priority) alist))
    (when-let ((due-date (ticktick-task-due-date task)))
      (push (cons "dueDate" due-date) alist))
    (when-let ((content (ticktick-task-content task)))
      (push (cons "content" content) alist))
    (nreverse alist)))

(defun ticktick-v1--project-to-internal (v1-project)
  "Convert V1 API project plist to internal project structure."
  (ticktick-project-create
   :id (plist-get v1-project :id)
   :name (plist-get v1-project :name)
   :color (plist-get v1-project :color)
   :view-mode (plist-get v1-project :viewMode)
   :kind (plist-get v1-project :kind)))

(defun ticktick-v1--internal-to-api-project (project)
  "Convert internal PROJECT structure to V1 API format (alist)."
  (let ((alist '()))
    (when-let ((id (ticktick-project-id project)))
      (push (cons "id" id) alist))
    (when-let ((name (ticktick-project-name project)))
      (push (cons "name" name) alist))
    (when-let ((color (ticktick-project-color project)))
      (push (cons "color" color) alist))
    (when-let ((view-mode (ticktick-project-view-mode project)))
      (push (cons "viewMode" view-mode) alist))
    (when-let ((kind (ticktick-project-kind project)))
      (push (cons "kind" kind) alist))
    (nreverse alist)))

;;; Backend Protocol Implementation ------------------------------------------

(cl-defmethod ticktick-backend-authenticate ((backend ticktick-v1-backend))
  "Authenticate V1 backend via OAuth2 flow."
  (unless (and (stringp ticktick-v1-client-id)
               (not (string-empty-p ticktick-v1-client-id))
               (stringp ticktick-v1-client-secret)
               (not (string-empty-p ticktick-v1-client-secret)))
    (user-error "ticktick-v1-client-id and ticktick-v1-client-secret must be set"))
  (ticktick-v1--start-callback-server)
  (setf (ticktick-v1-backend-oauth-state backend)
        (format "%06x" (random (expt 16 6))))
  (let* ((auth-url (concat "https://ticktick.com/oauth/authorize?"
                           (url-build-query-string
                            `(("client_id" ,ticktick-v1-client-id)
                              ("response_type" "code")
                              ("redirect_uri" ,ticktick-v1-redirect-uri)
                              ("scope" ,ticktick-v1-auth-scopes)
                              ("state" ,(ticktick-v1-backend-oauth-state backend)))))))
    (browse-url auth-url)
    (message "TickTick V1: opened browser for OAuth. Waiting for callback...")))

(cl-defmethod ticktick-backend-refresh-token ((backend ticktick-v1-backend))
  "Refresh V1 backend OAuth token."
  (ticktick-v1--refresh-token-internal backend))

(cl-defmethod ticktick-backend-token-valid-p ((backend ticktick-v1-backend))
  "Check if V1 backend has valid token."
  (when-let ((token (ticktick-v1-backend-token backend)))
    (and (plist-get token :access_token)
         (not (ticktick-v1-token-expired-p token)))))

(cl-defmethod ticktick-backend-fetch-projects ((backend ticktick-v1-backend))
  "Fetch all projects using V1 API."
  (let* ((inbox-project (ticktick-project-create
                         :id "inbox"
                         :name "Inbox"
                         :color "#F18181"
                         :view-mode "list"
                         :kind "TASK"))
         (projects-data (ticktick-v1-request backend "GET" "/open/v1/project"))
         (projects (mapcar #'ticktick-v1--project-to-internal projects-data)))
    (cons inbox-project projects)))

(cl-defmethod ticktick-backend-fetch-project-data ((backend ticktick-v1-backend) project-id)
  "Fetch project data for PROJECT-ID using V1 API."
  (ticktick-v1-request backend "GET"
                       (format "/open/v1/project/%s/data" project-id)))

(cl-defmethod ticktick-backend-fetch-tasks ((backend ticktick-v1-backend) project-id)
  "Fetch tasks for PROJECT-ID using V1 API."
  (let* ((project-data (ticktick-backend-fetch-project-data backend project-id))
         (tasks-data (plist-get project-data :tasks)))
    (mapcar #'ticktick-v1--task-to-internal tasks-data)))

(cl-defmethod ticktick-backend-create-task ((backend ticktick-v1-backend) task project-id)
  "Create TASK in PROJECT-ID using V1 API."
  (let* ((api-task (ticktick-v1--internal-to-api-task task))
         (api-task-with-project (append api-task `(("projectId" . ,project-id))))
         (response (ticktick-v1-request backend "POST" "/open/v1/task"
                                        api-task-with-project)))
    (when response
      (ticktick-v1--task-to-internal response))))

(cl-defmethod ticktick-backend-update-task ((backend ticktick-v1-backend) task task-id project-id)
  "Update TASK with TASK-ID in PROJECT-ID using V1 API."
  (let* ((api-task (ticktick-v1--internal-to-api-task task))
         (api-task-with-project (append api-task `(("projectId" . ,project-id))))
         (response (ticktick-v1-request backend "POST"
                                        (format "/open/v1/task/%s" task-id)
                                        api-task-with-project)))
    (when response
      (ticktick-v1--task-to-internal response))))

(cl-defmethod ticktick-backend-delete-task ((backend ticktick-v1-backend) task-id project-id)
  "Delete task TASK-ID from PROJECT-ID using V1 API."
  (ticktick-v1-request backend "DELETE"
                       (format "/open/v1/task/%s/%s" project-id task-id))
  t)

(cl-defmethod ticktick-backend-create-project ((backend ticktick-v1-backend) project)
  "Create PROJECT using V1 API."
  (let* ((api-project (ticktick-v1--internal-to-api-project project))
         (response (ticktick-v1-request backend "POST" "/open/v1/project"
                                        api-project)))
    (when response
      (ticktick-v1--project-to-internal response))))

(cl-defmethod ticktick-backend-update-project ((backend ticktick-v1-backend) project project-id)
  "Update PROJECT with PROJECT-ID using V1 API."
  (let* ((api-project (ticktick-v1--internal-to-api-project project))
         (response (ticktick-v1-request backend "POST"
                                        (format "/open/v1/project/%s" project-id)
                                        api-project)))
    (when response
      (ticktick-v1--project-to-internal response))))

(cl-defmethod ticktick-backend-delete-project ((backend ticktick-v1-backend) project-id)
  "Delete project PROJECT-ID using V1 API."
  (ticktick-v1-request backend "DELETE"
                       (format "/open/v1/project/%s" project-id))
  t)

(cl-defmethod ticktick-backend-batch-update-tasks ((backend ticktick-v1-backend) operations)
  "V1 doesn't support batch operations, fall back to individual calls.
OPERATIONS is ignored, returns error plist."
  (error "V1 backend does not support batch operations"))

(cl-defmethod ticktick-backend-supports-batch-p ((_backend ticktick-v1-backend))
  "V1 does not support batch operations."
  nil)

(cl-defmethod ticktick-backend-supports-cancelled-status-p ((_backend ticktick-v1-backend))
  "V1 does not support cancelled status."
  nil)

(cl-defmethod ticktick-backend-name ((_backend ticktick-v1-backend))
  "Return backend name."
  "V1 (OAuth2)")

;;; Initialize V1 Backend ----------------------------------------------------

(setq ticktick--v1-backend (ticktick-v1-backend-create))
(ticktick-v1--load-token ticktick--v1-backend)

(provide 'ticktick-v1)
;;; ticktick-v1.el ends here
