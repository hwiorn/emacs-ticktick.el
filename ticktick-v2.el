;;; ticktick-v2.el --- TickTick V2 (Username/Password) backend -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Paul Huang
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;; Keywords: tools, ticktick, v2
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
;; This file implements the TickTick V2 (unofficial) backend.
;; It provides username/password authentication with richer features
;; including batch operations and cancelled task status.
;;
;; WARNING: This is an UNOFFICIAL API reverse-engineered from the web app.
;; It may break without notice. Use at your own risk.
;;
;; Features:
;; - Username/password authentication
;; - Manual 2FA/TOTP code input
;; - Batch operations for efficiency
;; - Cancelled/Won't-do task status
;; - Session-based tokens

;;; Code:

(require 'ticktick-backend)
(require 'ticktick-common)
(require 'request)
(require 'json)
(require 'url)
(require 'cl-lib)

;;; Customization Variables --------------------------------------------------

(defcustom ticktick-v2-username ""
  "TickTick V2 username (email address)."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-v2-password ""
  "TickTick V2 password.
Note: Stored in plain text. Consider using auth-source instead."
  :type 'string
  :group 'ticktick)

(defcustom ticktick-v2-batch-threshold 3
  "Minimum number of changes to trigger batch API usage.
When syncing >= this many tasks, use batch endpoint for efficiency."
  :type 'integer
  :group 'ticktick)

(defcustom ticktick-v2-token-file
  (expand-file-name ".ticktick-v2-token"
                    (concat user-emacs-directory "ticktick/"))
  "File in which to store TickTick V2 session token."
  :type 'file
  :group 'ticktick)

;;; Utility Functions --------------------------------------------------------

(defun ticktick-v2--generate-object-id ()
  "Generate a MongoDB-style ObjectId (24 hex characters).
Format: 8-char timestamp + 5-char random + 3-char counter"
  (let* ((timestamp (format "%08x" (truncate (float-time))))
         (random-part (format "%010x" (random (expt 16 10))))
         (counter (format "%06x" (random (expt 16 6)))))
    (concat timestamp (substring random-part 0 10) (substring counter 0 6))))

;;; V2 Constants -------------------------------------------------------------

(defconst ticktick-v2-user-agent
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/117.0"
  "User-Agent header to mimic web browser.")

(defconst ticktick-v2-x-device
  "{\"platform\":\"web\",\"os\":\"Windows 10\",\"device\":\"Firefox 117.0\",\"name\":\"\",\"version\":4576,\"id\":\"64fc9b22cbb2c305b2df7ad6\",\"channel\":\"website\",\"campaign\":\"\",\"websocket\":\"6500a8a3bf02224e648ef8bd\"}"
  "X-Device header to mimic web client.
Values taken from pyticktick reference implementation.")

(defconst ticktick-v2-base-url "https://api.ticktick.com/api/v2"
  "Base URL for V2 API endpoints.")

;;; Backend Structure --------------------------------------------------------

(cl-defstruct (ticktick-v2-backend
               (:constructor ticktick-v2-backend-create)
               (:copier nil))
  "V2 backend structure."
  (token nil)
  (token-expiry nil))

;;; Token Persistence --------------------------------------------------------

(defun ticktick-v2--ensure-dir ()
  "Ensure token directory exists."
  (let ((dir (file-name-directory ticktick-v2-token-file)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun ticktick-v2--save-token (backend)
  "Save BACKEND token to file."
  (when-let ((token (ticktick-v2-backend-token backend)))
    (ticktick-v2--ensure-dir)
    (with-temp-file ticktick-v2-token-file
      (insert (prin1-to-string
               (list :token token
                     :expiry (ticktick-v2-backend-token-expiry backend)))))))

(defun ticktick-v2--load-token (backend)
  "Load token from file into BACKEND."
  (when (file-exists-p ticktick-v2-token-file)
    (with-temp-buffer
      (insert-file-contents ticktick-v2-token-file)
      (goto-char (point-min))
      (let ((data (read (current-buffer))))
        (setf (ticktick-v2-backend-token backend) (plist-get data :token))
        (setf (ticktick-v2-backend-token-expiry backend) (plist-get data :expiry))))))

;;; Authentication Functions -------------------------------------------------

(defun ticktick-v2--parse-json-maybe ()
  "Parse current buffer as JSON if non-empty; otherwise return nil."
  (goto-char (point-min))
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false :false))
    (if (zerop (buffer-size))
        nil
      (condition-case _ (json-read)
        (json-error nil)))))

(defun ticktick-v2--sign-in (username password)
  "Sign in with USERNAME and PASSWORD.
Returns plist with :token (from cookie) or :needVerify/:authId for 2FA."
  (let ((response-data nil)
        (cookie-token nil))
    (request (concat ticktick-v2-base-url "/user/signon?wc=true&remember=true")
      :type "POST"
      :headers `(("User-Agent" . ,ticktick-v2-user-agent)
                 ("X-Device" . ,ticktick-v2-x-device)
                 ("Content-Type" . "application/json"))
      :data (json-encode `(("username" . ,username)
                           ("password" . ,password)))
      :parser #'ticktick-v2--parse-json-maybe
      :sync t
      :success (cl-function
                (lambda (&key data response &allow-other-keys)
                  ;; Extract token from Set-Cookie header
                  (let* ((headers (request-response-header response "set-cookie"))
                         (cookie-header (if (listp headers) (car headers) headers)))
                    (when cookie-header
                      (when (string-match "t=\\([^;]+\\)" cookie-header)
                        (setq cookie-token (match-string 1 cookie-header)))))
                  (setq response-data data)))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (let ((status (and response (request-response-status-code response))))
                  ;; Parse error response body
                  (when response
                    (with-current-buffer (request-response--buffer response)
                      (setq response-data (ticktick-v2--parse-json-maybe))))
                  (message "V2 sign-in failed: HTTP %s - %s\nResponse: %S"
                           (or status "unknown")
                           (or error-thrown "unknown error")
                           response-data)))))
    ;; Return combined result with cookie token if available
    (when (or response-data cookie-token)
      (if cookie-token
          (if response-data
              (plist-put response-data :token cookie-token)
            (list :token cookie-token))
        response-data))))

(defun ticktick-v2--verify-2fa (auth-id totp-code)
  "Verify 2FA with AUTH-ID and TOTP-CODE.
Returns response plist with token or nil on failure."
  (let ((response-data nil)
        (cookie-token nil))
    (request (concat ticktick-v2-base-url "/user/sign/mfa/code/verify")
      :type "POST"
      :headers `(("User-Agent" . ,ticktick-v2-user-agent)
                 ("X-Device" . ,ticktick-v2-x-device)
                 ("Content-Type" . "application/json"))
      :data (json-encode `(("authId" . ,auth-id)
                           ("code" . ,totp-code)))
      :parser #'ticktick-v2--parse-json-maybe
      :sync t
      :success (cl-function
                (lambda (&key data response &allow-other-keys)
                  ;; Extract token from Set-Cookie header
                  (let* ((headers (request-response-header response "set-cookie"))
                         (cookie-header (if (listp headers) (car headers) headers)))
                    (when cookie-header
                      (when (string-match "t=\\([^;]+\\)" cookie-header)
                        (setq cookie-token (match-string 1 cookie-header)))))
                  (setq response-data data)))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (let ((status (and response (request-response-status-code response))))
                  ;; Parse error response body
                  (when response
                    (with-current-buffer (request-response--buffer response)
                      (setq response-data (ticktick-v2--parse-json-maybe))))
                  (message "V2 2FA verification failed: HTTP %s - %s\nResponse: %S"
                           (or status "unknown")
                           (or error-thrown "unknown error")
                           response-data)))))
    ;; Return combined result with cookie token if available
    (when (or response-data cookie-token)
      (if cookie-token
          (if response-data
              (plist-put response-data :token cookie-token)
            (list :token cookie-token))
        response-data))))

(defun ticktick-v2--authenticate-internal (backend)
  "Perform V2 authentication for BACKEND.
Prompts for credentials if not set."
  (let* ((username (or ticktick-v2-username
                       (read-string "TickTick email: ")))
         (password (or ticktick-v2-password
                       (read-passwd "TickTick password: ")))
         (response (ticktick-v2--sign-in username password)))

    (unless ticktick-v2-username
      (setq ticktick-v2-username username))
    (unless ticktick-v2-password
      (setq ticktick-v2-password password))

    (cond
     ;; 2FA required
     ((plist-get response :needVerify)
      (let* ((auth-id (plist-get response :authId))
             (totp-code (read-string "Enter TOTP code from authenticator app: "))
             (verify-response (ticktick-v2--verify-2fa auth-id totp-code))
             (token (plist-get verify-response :token)))
        (if token
            (progn
              (setf (ticktick-v2-backend-token backend) token)
              (setf (ticktick-v2-backend-token-expiry backend)
                    (+ (float-time) (* 24 60 60)))  ; Assume 24h expiry
              (ticktick-v2--save-token backend)
              (message "V2: Authenticated successfully with 2FA"))
          (error "V2: 2FA verification failed"))))

     ;; Direct token (no 2FA)
     ((plist-get response :token)
      (let ((token (plist-get response :token)))
        (setf (ticktick-v2-backend-token backend) token)
        (setf (ticktick-v2-backend-token-expiry backend)
              (+ (float-time) (* 24 60 60)))  ; Assume 24h expiry
        (ticktick-v2--save-token backend)
        (message "V2: Authenticated successfully")))

     ;; Error
     (t
      (let* ((error-code (plist-get response :errorCode))
             (error-msg (plist-get response :errorMessage))
             (error-id (plist-get response :errorId))
             (friendly-msg
              (pcase error-code
                ("username_password_not_match"
                 "Username or password is incorrect. Please verify your credentials.")
                ("user_not_exist"
                 "User account does not exist.")
                ("account_locked"
                 "Account has been locked. Please contact TickTick support.")
                (_ (or error-msg "Unknown authentication error")))))
        (message "V2: Authentication failed\n  Error Code: %s\n  Error ID: %s\n  Response: %S"
                 (or error-code "none")
                 (or error-id "none")
                 response)
        (error "V2: %s (code: %s)"
               friendly-msg
               (or error-code "unknown")))))))

(defun ticktick-v2--token-valid-p (backend)
  "Check if BACKEND token is valid and not expired."
  (and (ticktick-v2-backend-token backend)
       (ticktick-v2-backend-token-expiry backend)
       (> (ticktick-v2-backend-token-expiry backend) (float-time))))

(defun ticktick-v2--ensure-token (backend)
  "Ensure BACKEND has a valid token, re-authenticate if needed."
  (ticktick-v2--load-token backend)
  (unless (ticktick-v2--token-valid-p backend)
    (ticktick-v2--authenticate-internal backend)))

;;; API Request Functions ----------------------------------------------------

(defun ticktick-v2-request (backend method endpoint &optional data skip-token-check)
  "Send a V2 API request using BACKEND.
METHOD is the HTTP method, ENDPOINT is the API path, DATA is optional body.
SKIP-TOKEN-CHECK skips the token validation (used for retry after re-auth)."
  (unless skip-token-check
    (ticktick-v2--ensure-token backend))
  (let* ((url (if (string-prefix-p "http" endpoint)
                  endpoint
                (concat ticktick-v2-base-url endpoint)))
         (token (ticktick-v2-backend-token backend))
         (headers `(("User-Agent" . ,ticktick-v2-user-agent)
                    ("X-Device" . ,ticktick-v2-x-device)
                    ("Content-Type" . "application/json")
                    ("Cookie" . ,(format "t=%s" token))))
         (json-data (and data (json-encode data)))
         (response-data nil))
    (request url
      :type method
      :headers headers
      :data json-data
      :parser #'ticktick-v2--parse-json-maybe
      :sync t
      :success (cl-function
                (lambda (&key data response &allow-other-keys)
                  (let ((status (request-response-status-code response)))
                    (cond
                     ((and (>= status 200) (< status 300))
                      (setq response-data data))
                     ((= status 401)
                      (message "V2: Token expired, re-authenticating...")
                      (ticktick-v2--authenticate-internal backend)
                      (setq response-data (ticktick-v2-request backend method endpoint data t)))
                     (t (error "V2 HTTP Error %s" status))))))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (let ((status (and response (request-response-status-code response))))
                  (cond
                   ((= status 401)
                    (message "V2: Token expired, re-authenticating...")
                    (ticktick-v2--authenticate-internal backend)
                    (setq response-data (ticktick-v2-request backend method endpoint data t)))
                   (t
                    (message "V2 request failed: %s"
                             (or error-thrown "unknown error"))
                    (setq response-data nil)))))))
    response-data))

(defun ticktick-v2-delete-request (backend endpoint &optional params skip-token-check)
  "Send a V2 DELETE request using BACKEND.
ENDPOINT is the API path, PARAMS is optional query parameters (alist).
SKIP-TOKEN-CHECK skips the token validation (used for retry after re-auth)."
  (unless skip-token-check
    (ticktick-v2--ensure-token backend))
  (let* ((url (if (string-prefix-p "http" endpoint)
                  endpoint
                (concat ticktick-v2-base-url endpoint)))
         (token (ticktick-v2-backend-token backend))
         (headers `(("User-Agent" . ,ticktick-v2-user-agent)
                    ("X-Device" . ,ticktick-v2-x-device)
                    ("Cookie" . ,(format "t=%s" token))))
         (response-success nil))
    (request url
      :type "DELETE"
      :headers headers
      :params params
      :parser #'ticktick-v2--parse-json-maybe
      :sync t
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (let ((status (request-response-status-code response)))
                    (cond
                     ((and (>= status 200) (< status 300))
                      (setq response-success t))
                     ((= status 401)
                      (message "V2: Token expired, re-authenticating...")
                      (ticktick-v2--authenticate-internal backend)
                      (setq response-success (ticktick-v2-delete-request backend endpoint params t)))
                     (t (error "V2 HTTP Error %s" status))))))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (let ((status (and response (request-response-status-code response))))
                  (cond
                   ((= status 401)
                    (message "V2: Token expired, re-authenticating...")
                    (ticktick-v2--authenticate-internal backend)
                    (setq response-success (ticktick-v2-delete-request backend endpoint params t)))
                   (t
                    (message "V2 delete request failed: %s"
                             (or error-thrown "unknown error"))
                    (setq response-success nil)))))))
    response-success))

;;; V2 <-> Internal Conversion -----------------------------------------------

(defun ticktick-v2--status-to-internal (v2-status)
  "Convert V2 integer status (-1/0/1/2) to internal symbol."
  (pcase v2-status
    (0 ticktick-status-active)
    (1 ticktick-status-completed)
    (2 ticktick-status-completed)
    (-1 ticktick-status-cancelled)
    (_ ticktick-status-active)))

(defun ticktick-v2--internal-to-status (internal-status)
  "Convert internal status symbol to V2 integer."
  (pcase internal-status
    ('active 0)
    ('completed 2)
    ('cancelled -1)
    (_ 0)))

(defun ticktick-v2--task-to-internal (v2-task)
  "Convert V2 API task plist to internal task structure."
  (ticktick-task-create
   :id (plist-get v2-task :id)
   :title (plist-get v2-task :title)
   :status (ticktick-v2--status-to-internal (plist-get v2-task :status))
   :priority (or (plist-get v2-task :priority) 0)
   :due-date (plist-get v2-task :dueDate)
   :content (or (plist-get v2-task :content)
                (plist-get v2-task :desc))
   :etag (plist-get v2-task :etag)
   :project-id (plist-get v2-task :projectId)
   :sort-order (plist-get v2-task :sortOrder)
   :tags (plist-get v2-task :tags)
   :kind (or (plist-get v2-task :kind) "TEXT")
   :created-time (plist-get v2-task :createdTime)
   :modified-time (plist-get v2-task :modifiedTime)
   :completed-time (plist-get v2-task :completedTime)))

(defun ticktick-v2--internal-to-api-task (task &optional for-creation)
  "Convert internal TASK structure to V2 API format (alist).
If FOR-CREATION is non-nil, generate a new ID for tasks without one."
  (let ((alist '())
        (internal-status (ticktick-task-status task))
        (api-status (ticktick-v2--internal-to-status (ticktick-task-status task))))
    ;; ID: Required - generate if creating and no ID exists
    (let ((id (ticktick-task-id task)))
      (when (or id for-creation)
        (push (cons "id" (or id (ticktick-v2--generate-object-id))) alist)))

    ;; Title: Required for V2 API
    (let ((title (ticktick-task-title task)))
      (push (cons "title" (or title "Untitled")) alist))

    ;; Status: Always include
    (push (cons "status" api-status) alist)

    ;; Optional fields
    (when-let ((priority (ticktick-task-priority task)))
      (push (cons "priority" priority) alist))
    (when-let ((due-date (ticktick-task-due-date task)))
      (push (cons "dueDate" due-date) alist))
    (when-let ((content (ticktick-task-content task)))
      (push (cons "content" content) alist))
    (let ((sort-order (ticktick-task-sort-order task)))
      (when sort-order
        (message "V2 DEBUG: Adding sortOrder=%s for task: %s" sort-order (ticktick-task-title task))
        (push (cons "sortOrder" sort-order) alist)))
    (when-let ((completed-time (ticktick-task-completed-time task)))
      (push (cons "completedTime" completed-time) alist))

    (let ((result (nreverse alist)))
      (message "V2 DEBUG: Final API task alist: %S" result)
      result)))

(defun ticktick-v2--project-to-internal (v2-project)
  "Convert V2 API project plist to internal project structure."
  (ticktick-project-create
   :id (plist-get v2-project :id)
   :name (plist-get v2-project :name)
   :color (plist-get v2-project :color)
   :view-mode (plist-get v2-project :viewMode)
   :kind (plist-get v2-project :kind)))

(defun ticktick-v2--internal-to-api-project (project &optional for-creation)
  "Convert internal PROJECT structure to V2 API format (alist).
If FOR-CREATION is non-nil, generate a new ID for projects without one."
  (let ((alist '()))
    ;; ID: Required - generate if creating and no ID exists
    (let ((id (ticktick-project-id project)))
      (when (or id for-creation)
        (let ((project-id (or id (ticktick-v2--generate-object-id))))
          (push (cons "id" project-id) alist)
          ;; Store the generated ID back in the project
          (when (and for-creation (not id))
            (setf (ticktick-project-id project) project-id)))))

    ;; Name: Required
    (let ((name (ticktick-project-name project)))
      (push (cons "name" (or name "Untitled")) alist))

    ;; Optional fields
    (when-let ((color (ticktick-project-color project)))
      (push (cons "color" color) alist))
    (when-let ((view-mode (ticktick-project-view-mode project)))
      (push (cons "viewMode" view-mode) alist))
    (when-let ((kind (ticktick-project-kind project)))
      (push (cons "kind" kind) alist))
    (nreverse alist)))

;;; Batch Operations ---------------------------------------------------------

(defun ticktick-v2-batch-task (backend operations)
  "Perform batch task operations using BACKEND.
OPERATIONS is a plist with :add, :update, :delete keys containing task alists.
Returns a plist with :id2etag (successes) and :id2error (failures)."
  (let* ((add-tasks (plist-get operations :add))
         (update-tasks (plist-get operations :update))
         (delete-tasks (plist-get operations :delete))
         (data '()))
    (when add-tasks
      (push (cons "add" add-tasks) data))
    (when update-tasks
      (push (cons "update" update-tasks) data))
    (when delete-tasks
      (push (cons "delete" delete-tasks) data))
    (when data
      (ticktick-v2-request backend "POST" "/batch/task" (nreverse data)))))

;;; Backend Protocol Implementation ------------------------------------------

(cl-defmethod ticktick-backend-authenticate ((backend ticktick-v2-backend))
  "Authenticate V2 backend via username/password."
  (ticktick-v2--authenticate-internal backend))

(cl-defmethod ticktick-backend-refresh-token ((backend ticktick-v2-backend))
  "Re-authenticate V2 backend (no refresh, must re-login)."
  (ticktick-v2--authenticate-internal backend))

(cl-defmethod ticktick-backend-token-valid-p ((backend ticktick-v2-backend))
  "Check if V2 backend has valid token."
  (ticktick-v2--token-valid-p backend))

(cl-defmethod ticktick-backend-fetch-projects ((backend ticktick-v2-backend))
  "Fetch all projects using V2 batch API."
  (let* ((batch-data (ticktick-v2-request backend "GET" "/batch/check/0"))
           (inbox-id (plist-get batch-data :inboxId))
           (inbox-project (ticktick-project-create
                       :id inbox-id
                       :name "Inbox"
                       :color "#F18181"
                       :view-mode "list"
                       :kind "TASK"))
           (projects-data (plist-get batch-data :projectProfiles))
           (projects (mapcar #'ticktick-v2--project-to-internal projects-data)))
    (cons inbox-project projects)))

(cl-defmethod ticktick-backend-fetch-project-data ((backend ticktick-v2-backend) project-id)
  "Fetch project data for PROJECT-ID using V2 batch API."
  (let ((batch-data (ticktick-v2-request backend "GET" "/batch/check/0")))
    ;; Find project and its tasks from batch data
    (let* ((all-tasks (plist-get (plist-get batch-data :syncTaskBean) :update))
             (project-tasks (cl-remove-if-not
                         (lambda (task)
                           (string= (plist-get task :projectId) project-id))
                         all-tasks)))
      `(:tasks ,project-tasks))))

(cl-defmethod ticktick-backend-fetch-tasks ((backend ticktick-v2-backend) project-id)
  "Fetch tasks for PROJECT-ID using V2 batch API."
  (let* ((project-data (ticktick-backend-fetch-project-data backend project-id))
           (tasks-data (plist-get project-data :tasks)))
    (mapcar #'ticktick-v2--task-to-internal tasks-data)))

(cl-defmethod ticktick-backend-create-task ((backend ticktick-v2-backend) task project-id)
  "Create TASK in PROJECT-ID using V2 API."
  (let* ((api-task (ticktick-v2--internal-to-api-task task t))  ; t = for-creation
           (api-task-with-project (append api-task `(("projectId" . ,project-id))))
           (response (ticktick-v2-batch-task backend
                                         `(:add (,api-task-with-project)))))
    (when response
      (message "V2: Create task response: %S" response)
      (let ((id2etag (plist-get response :id2etag)))
        (message "V2: id2etag: %S" id2etag)
        (when id2etag
          ;; For newly created task, get the first (and only) id from id2etag
          ;; id2etag is a plist with keyword keys: (:task-id "etag" ...)
          ;; Get the generated ID from api-task
          (let* ((generated-id (cdr (assoc "id" api-task)))
                 ;; Convert string ID to keyword for plist lookup
                 (id-keyword (intern (concat ":" generated-id)))
                 (new-etag (plist-get id2etag id-keyword)))
            (message "V2: generated-id=%s, id-keyword=%s, new-etag=%s"
                     generated-id id-keyword new-etag)
            (when generated-id
              (setf (ticktick-task-id task) generated-id)
              (setf (ticktick-task-etag task) new-etag)
              task)))))))

(cl-defmethod ticktick-backend-update-task ((backend ticktick-v2-backend) task task-id project-id)
  "Update TASK with TASK-ID in PROJECT-ID using V2 API."
  (let* ((api-task (ticktick-v2--internal-to-api-task task))
           (api-task-with-project (append api-task
                                      `(("projectId" . ,project-id)
                                        ("id" . ,task-id))))
           (response (ticktick-v2-batch-task backend
                                         `(:update (,api-task-with-project)))))
    (when response
      (let ((id2etag (plist-get response :id2etag)))
        (when id2etag
          ;; Convert string ID to keyword for plist lookup
          (let ((id-keyword (intern (concat ":" task-id))))
            (setf (ticktick-task-etag task)
                  (plist-get id2etag id-keyword))
            task))))))

(cl-defmethod ticktick-backend-delete-task ((backend ticktick-v2-backend) task-id project-id)
  "Delete task TASK-ID from PROJECT-ID using V2 API."
  (ticktick-v2-batch-task backend
                          `(:delete ((("taskId" . ,task-id)
                                      ("projectId" . ,project-id)))))
  t)

(cl-defmethod ticktick-backend-create-project ((backend ticktick-v2-backend) project)
  "Create PROJECT using V2 API."
  (let* ((api-project (ticktick-v2--internal-to-api-project project t))  ; t = for-creation
           (response (ticktick-v2-request backend "POST" "/batch/project"
                                      `(("add" . (,api-project))))))
    (when response
      ;; The project object now has the generated ID from internal-to-api-project
      ;; Return the project with the ID
      project)))

(cl-defmethod ticktick-backend-update-project ((backend ticktick-v2-backend) project project-id)
  "Update PROJECT with PROJECT-ID using V2 API."
  (let* ((api-project (ticktick-v2--internal-to-api-project project))
           (api-project-with-id (append api-project `(("id" . ,project-id))))
           (response (ticktick-v2-request backend "POST" "/batch/project"
                                      `(("update" . (,api-project-with-id))))))
    (when response
      project)))

(cl-defmethod ticktick-backend-delete-project ((backend ticktick-v2-backend) project-id)
  "Delete project PROJECT-ID using V2 API.
Format matches Python API: {\"delete\": [\"project_id\"]}"
  (ticktick-v2-request backend "POST" "/batch/project"
                       `(("delete" . (,project-id))))
  t)

(defun ticktick-v2-delete-tag (backend tag-name)
  "Delete tag with TAG-NAME using V2 API.
Uses DELETE /tag?name=TAG-NAME endpoint."
  (ticktick-v2-delete-request backend "/tag" `(("name" . ,tag-name))))

(cl-defmethod ticktick-backend-batch-update-tasks ((backend ticktick-v2-backend) operations)
  "Perform batch task operations using V2 batch API.
OPERATIONS is a plist with :add, :update, :delete keys."
  (ticktick-v2-batch-task backend operations))

(cl-defmethod ticktick-backend-supports-batch-p ((_backend ticktick-v2-backend))
  "V2 supports batch operations."
  t)

(cl-defmethod ticktick-backend-supports-cancelled-status-p ((_backend ticktick-v2-backend))
  "V2 supports cancelled status."
  t)

(cl-defmethod ticktick-backend-name ((_backend ticktick-v2-backend))
  "Return backend name."
  "V2 (Username/Password)")

;;; Initialize V2 Backend ----------------------------------------------------

(setq ticktick--v2-backend (ticktick-v2-backend-create))
(ticktick-v2--load-token ticktick--v2-backend)

;;; Debug Helpers ------------------------------------------------------------

;;;###autoload
(defun ticktick-v2-debug-auth ()
  "Debug V2 authentication by testing sign-in and showing response.
This is useful for troubleshooting authentication issues."
  (interactive)
  (let* ((username (read-string "TickTick email: "))
           (password (read-passwd "TickTick password: "))
           (response (ticktick-v2--sign-in username password)))
    (with-current-buffer (get-buffer-create "*TickTick V2 Debug*")
      (erase-buffer)
      (insert "=== TickTick V2 Authentication Debug ===\n\n")
      (insert (format "Username: %s\n" username))
      (insert (format "Password: %s\n\n" (make-string (length password) ?*)))
      (insert "Response:\n")
      (insert (pp-to-string response))
      (insert "\n\nToken extracted: ")
      (insert (if (plist-get response :token)
                  (format "YES (%d characters)" (length (plist-get response :token)))
                "NO"))
      (insert "\n\nNeed 2FA: ")
      (insert (if (plist-get response :needVerify) "YES" "NO"))
      (when (plist-get response :authId)
        (insert (format "\nAuth ID: %s" (plist-get response :authId))))
      (goto-char (point-min))
      (special-mode)
      (display-buffer (current-buffer)))))

(provide 'ticktick-v2)
;;; ticktick-v2.el ends here
