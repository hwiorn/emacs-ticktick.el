;;; ticktick-backend.el --- Backend abstraction for TickTick API -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Paul Huang
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ticktick, backend
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
;; This file defines the backend abstraction protocol for TickTick API.
;; It provides a common interface that both V1 (OAuth2) and V2 (Username/Password)
;; backends implement.
;;
;; The protocol uses cl-defgeneric to define polymorphic functions that
;; dispatch based on the backend type.

;;; Code:

(require 'cl-lib)

(defgroup ticktick nil
  "Interface with TickTick API."
  :prefix "ticktick-"
  :group 'applications)

;;; Backend Protocol Definition -----------------------------------------------

(cl-defgeneric ticktick-backend-authenticate (backend)
  "Authenticate with the TickTick service using BACKEND.
Returns non-nil on success, signals error on failure.")

(cl-defgeneric ticktick-backend-refresh-token (backend)
  "Refresh authentication token for BACKEND.
Returns non-nil on success, signals error on failure.")

(cl-defgeneric ticktick-backend-token-valid-p (backend)
  "Check if BACKEND has a valid authentication token.
Returns non-nil if token is valid, nil otherwise.")

(cl-defgeneric ticktick-backend-fetch-projects (backend)
  "Fetch all projects from TickTick using BACKEND.
Returns a list of project structures in internal format.")

(cl-defgeneric ticktick-backend-fetch-project-data (backend project-id)
  "Fetch detailed data for PROJECT-ID using BACKEND.
Returns a plist with :project and :tasks keys.")

(cl-defgeneric ticktick-backend-fetch-tasks (backend project-id)
  "Fetch all tasks for PROJECT-ID using BACKEND.
Returns a list of task structures in internal format.")

(cl-defgeneric ticktick-backend-create-task (backend task project-id)
  "Create TASK in PROJECT-ID using BACKEND.
TASK is an internal task structure.
Returns the created task with server-assigned ID and etag.")

(cl-defgeneric ticktick-backend-update-task (backend task task-id project-id)
  "Update TASK with TASK-ID in PROJECT-ID using BACKEND.
TASK is an internal task structure.
Returns the updated task with new etag.")

(cl-defgeneric ticktick-backend-delete-task (backend task-id project-id)
  "Delete task with TASK-ID from PROJECT-ID using BACKEND.
Returns non-nil on success.")

(cl-defgeneric ticktick-backend-create-project (backend project)
  "Create PROJECT using BACKEND.
PROJECT is an internal project structure.
Returns the created project with server-assigned ID.")

(cl-defgeneric ticktick-backend-update-project (backend project project-id)
  "Update PROJECT with PROJECT-ID using BACKEND.
PROJECT is an internal project structure.
Returns the updated project.")

(cl-defgeneric ticktick-backend-delete-project (backend project-id)
  "Delete project with PROJECT-ID using BACKEND.
Returns non-nil on success.")

(cl-defgeneric ticktick-backend-batch-update-tasks (backend operations)
  "Perform batch OPERATIONS on tasks using BACKEND.
OPERATIONS is a plist with :add, :update, :delete keys, each containing
a list of task structures in internal format.

This is an optional optimization. Backends that don't support batch
operations can fall back to individual operations.

Returns a plist with :success and :errors keys.")

(cl-defgeneric ticktick-backend-supports-batch-p (backend)
  "Check if BACKEND supports batch operations.
Returns non-nil if batch operations are supported.")

(cl-defgeneric ticktick-backend-supports-cancelled-status-p (backend)
  "Check if BACKEND supports cancelled/won't-do task status.
Returns non-nil if supported (V2), nil otherwise (V1).")

(cl-defgeneric ticktick-backend-name (backend)
  "Return a human-readable name for BACKEND (e.g., 'V1 (OAuth2)').")

;;; Backend Selection and Management -----------------------------------------

(defcustom ticktick-backend-type 'v1
  "Backend to use for TickTick API.
- 'v1: Official OAuth2 API (stable, requires app registration)
- 'v2: Unofficial username/password API (more features, may break)"
  :type '(choice (const :tag "V1 (OAuth2)" v1)
          (const :tag "V2 (Username/Password)" v2))
  :group 'ticktick)

(defvar ticktick--v1-backend nil
  "Instance of V1 backend.")

(defvar ticktick--v2-backend nil
  "Instance of V2 backend.")

(defun ticktick--get-backend ()
  "Get the current backend instance based on `ticktick-backend-type'."
  (pcase ticktick-backend-type
    ('v1 (or ticktick--v1-backend
             (user-error "V1 backend not initialized. Please require ticktick-v1")))
    ('v2 (or ticktick--v2-backend
             (progn
               (require 'ticktick-v2)
               (or ticktick--v2-backend
                   (user-error "V2 backend not initialized")))))
    (_ (user-error "Unknown backend type: %s" ticktick-backend-type))))

(defun ticktick--backend-call (method &rest args)
  "Call METHOD on current backend with ARGS.
METHOD should be a symbol naming a backend protocol function."
  (let ((backend (ticktick--get-backend)))
    (apply method backend args)))

(defun ticktick--init-backend ()
  "Initialize the selected backend if not already initialized."
  (pcase ticktick-backend-type
    ('v1 (unless ticktick--v1-backend
           (require 'ticktick-v1)
           (setq ticktick--v1-backend (ticktick-v1-backend-create))))
    ('v2 (unless ticktick--v2-backend
           (require 'ticktick-v2)
           (setq ticktick--v2-backend (ticktick-v2-backend-create))))))

;;; Backend Information Functions --------------------------------------------

(defun ticktick-backend-info ()
  "Display information about the current backend."
  (interactive)
  (ticktick--init-backend)
  (let* ((backend (ticktick--get-backend))
         (name (ticktick-backend-name backend))
         (valid (ticktick-backend-token-valid-p backend))
         (supports-batch (ticktick-backend-supports-batch-p backend))
         (supports-cancelled (ticktick-backend-supports-cancelled-status-p backend)))
    (message "TickTick Backend: %s\nAuthenticated: %s\nBatch operations: %s\nCancelled status: %s"
             name
             (if valid "Yes" "No")
             (if supports-batch "Yes" "No")
             (if supports-cancelled "Yes" "No"))))

(provide 'ticktick-backend)
;;; ticktick-backend.el ends here
