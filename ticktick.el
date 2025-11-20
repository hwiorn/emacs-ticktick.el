;;; ticktick.el --- Sync Org Mode tasks with TickTick -*- lexical-binding: t; -*-

;; Author: Paul Huang
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (simple-httpd "1.5.0") (org "9.0"))
;; Keywords: tools, ticktick, org, tasks, todo
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
;; ticktick.el provides two-way synchronization between TickTick
;; (a popular task management service) and Emacs Org Mode.
;;
;; FEATURES:
;;
;; - Bidirectional sync: changes in either TickTick or Org Mode are reflected
;;   in both systems
;; - Dual backend support: V1 (OAuth2) and V2 (Username/Password)
;; - Preserves task metadata: priorities, due dates, completion status
;; - Project-based organization matching TickTick's structure
;; - Optional automatic syncing on focus changes
;; - Multi-file synchronization across org files
;;
;; SETUP:
;;
;; For V1 (OAuth2 - recommended, official API):
;; 1. Register a TickTick OAuth application at:
;;    https://developer.ticktick.com/
;; 2. Configure your credentials:
;;    (setq ticktick-v1-client-id "your-client-id")
;;    (setq ticktick-v1-client-secret "your-client-secret")
;; 3. Authorize the application:
;;    M-x ticktick-authorize
;;
;; For V2 (Username/Password - unofficial, more features):
;; 1. Switch to V2 backend:
;;    M-x ticktick-switch-to-v2
;; 2. Set credentials (prompted):
;;    Email and password
;; 3. Authorize:
;;    M-x ticktick-authorize
;;
;; 4. Perform initial sync:
;;    M-x ticktick-sync
;;
;; USAGE:
;;
;; Main commands:
;; - `ticktick-sync': Full bidirectional sync
;; - `ticktick-fetch-to-org': Pull tasks from TickTick to Org
;; - `ticktick-push-from-org': Push Org tasks to TickTick
;; - `ticktick-authorize': Set up authentication
;; - `ticktick-refresh-token': Manually refresh auth token (V1 only)
;; - `ticktick-switch-to-v1': Switch to V1 (OAuth2) backend
;; - `ticktick-switch-to-v2': Switch to V2 (Username/Password) backend
;; - `ticktick-backend-info': Show current backend information
;; - `ticktick-toggle-sync-timer': Toggle automatic timer-based syncing
;; - `ticktick-create-project': Create a new TickTick project
;; - `ticktick-update-project': Update current project properties
;; - `ticktick-delete-project': Delete current project
;; - `ticktick-delete-all-projects': Delete ALL projects from TickTick (dangerous!)
;; - `ticktick-delete-all-tags': Delete ALL tags from TickTick (V2 only, dangerous!)
;; - `ticktick-enable-multi-file': Enable multi-file synchronization
;; - `ticktick-disable-multi-file': Disable multi-file synchronization
;;
;; BACKEND COMPARISON:
;;
;; V1 (OAuth2):
;;  + Official, stable API
;;  + Long-lived tokens (~6 months)
;;  + No password storage
;;  - Requires app registration
;;  - Binary task status only (TODO/DONE)
;;  - No batch operations
;;
;; V2 (Username/Password):
;;  + No app registration needed
;;  + Richer features (batch operations, cancelled status)
;;  + Three-state status (TODO/DONE/CANCELLED)
;;  - Unofficial API (may break)
;;  - Session-based tokens (shorter lifespan)
;;  - Stores password in plain text
;;
;; CUSTOMIZATION:
;;
;; Key variables you can customize:
;; - `ticktick-backend-type': Which backend to use ('v1 or 'v2)
;; - `ticktick-sync-file': Path to the org file for tasks
;; - `ticktick-multi-file-support': Enable sync across multiple files
;; - `ticktick-autosync': Enable automatic syncing on focus changes
;; - `ticktick-sync-interval': Enable automatic syncing every N minutes
;;
;; For V1-specific settings, see ticktick-v1.el
;; For V2-specific settings, see ticktick-v2.el

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'subr-x)
(require 'ticktick-backend)
(require 'ticktick-common)
(require 'ticktick-v1)  ; V1 is default, always load

;;; Configuration ------------------------------------------------------------

(defcustom ticktick-sync-file
  (expand-file-name "ticktick.org"
                    (concat user-emacs-directory "ticktick/"))
  "Path to the org file where all TickTick tasks will be synchronized.
Can be either:
- A single file path (string)
- A list of file paths (for multi-file support without directory scanning)"
  :type '(choice file (repeat file))
  :group 'ticktick)

(defcustom ticktick-multi-file-support nil
  "If non-nil, enable multi-file synchronization across all configured org files."
  :type 'boolean
  :group 'ticktick)

(defcustom ticktick-project-detection-function 'ticktick--project-p-default
  "Function to determine if an org heading represents a TickTick project.
The function should take no arguments and return non-nil if the current
heading is a project that should be synchronized with TickTick."
  :type 'function
  :group 'ticktick)

(defcustom ticktick-project-name-function 'ticktick--project-name-default
  "Function to extract the project name from an org heading.
The function should take no arguments and return the project name as a string."
  :type 'function
  :group 'ticktick)

(defcustom ticktick-org-file-directories '("~/.config/doom/lisp/ticktick.el/testdata")
  "List of directories to scan for org files when multi-file support is enabled.
Files in these directories will be checked for TickTick projects."
  :type '(repeat directory)
  :group 'ticktick)

(defcustom ticktick-org-file-patterns '("*.org")
  "File patterns to match when scanning for org files.
Only files matching these patterns will be checked for projects."
  :type '(repeat string)
  :group 'ticktick)

(defcustom ticktick-autosync nil
  "If non-nil, automatically sync when switching buffers or losing focus."
  :type 'boolean
  :group 'ticktick)

(defcustom ticktick-sync-interval nil
  "Interval in minutes for automatic syncing. If nil, timer-based sync is disabled.
When set to a positive number, TickTick will sync automatically every N minutes.
After changing this value, call `ticktick-toggle-sync-timer' to apply changes."
  :type '(choice (const :tag "Disabled" nil)
          (integer :tag "Minutes"))
  :group 'ticktick)

(defvar ticktick--sync-timer nil
  "Timer object for periodic syncing.")

;;; Utility Functions --------------------------------------------------------

(defun ticktick--ensure-backend ()
  "Ensure the selected backend is initialized."
  (ticktick--init-backend))

;;; Sync Helper Functions ----------------------------------------------------

(defun ticktick--create-project-heading (project)
  "Insert a new Org heading for PROJECT (internal struct).
Return the buffer position at the start of the heading."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (let ((start (point)))
    (insert (ticktick-common-project-to-org project))
    start))

(defun ticktick--sync-task (task project-pos)
  "Sync a single TASK (internal struct) under PROJECT-POS."
  (let* ((id (ticktick-task-id task))
         (etag (ticktick-task-etag task))
         (existing-pos (ticktick--find-task-under-project project-pos id)))
    (if existing-pos
        (save-excursion
          (goto-char existing-pos)
          (let ((existing-etag (org-entry-get nil "TICKTICK_ETAG")))
            (unless (and etag existing-etag (string= existing-etag etag))
              (delete-region (org-entry-beginning-position)
                             (org-entry-end-position))
              (insert (ticktick-common-task-to-org task))
              (ticktick-common-update-sync-meta))))
      (save-excursion
        (goto-char project-pos)
        (outline-next-heading)
        (insert (ticktick-common-task-to-org task) "\n")
        (ticktick-common-update-sync-meta)))))

(defun ticktick--sync-project (project backend)
  "Sync a single PROJECT (internal struct) using BACKEND."
  (let* ((project-id (ticktick-project-id project))
         (project-title (ticktick-project-name project))
         (project-heading-re (format "^\\* %s$" (regexp-quote project-title)))
         (project-pos (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward project-heading-re nil t)
                          (match-beginning 0)))))
    (unless project-pos
      (setq project-pos (ticktick--create-project-heading project)))
    (goto-char project-pos)
    (outline-show-subtree)
    (let ((tasks (ticktick-backend-fetch-tasks backend project-id)))
      (dolist (task tasks)
        (ticktick--sync-task task project-pos)))))

(defun ticktick--find-task-under-project (project-heading id)
  "Return position of task heading with ID under PROJECT-HEADING."
  (save-excursion
    (goto-char project-heading)
    (catch 'found
      (org-map-entries
       (lambda ()
         (when (string= (org-entry-get nil "TICKTICK_ID") id)
           (throw 'found (point))))
       nil 'tree)
      nil)))

(defun ticktick--find-project-heading (project-name project-id)
  "Find existing project heading by name or ID.
Returns buffer position if found, nil otherwise.
Tries to match by ID first, then falls back to name-only matching if local ID is empty."
  (save-excursion
    (goto-char (point-min))
    (let ((name-regex (format "^\\* %s$" (regexp-quote project-name)))
          (found-pos nil))
      (while (and (not found-pos) (re-search-forward name-regex nil t))
        (let ((pos (match-beginning 0)))
          (save-excursion
            (goto-char pos)
            (let ((local-id (org-entry-get nil "TICKTICK_PROJECT_ID")))
              ;; Match if:
              ;; 1. No project-id provided (name-only search), OR
              ;; 2. Local ID is nil or empty (match by name), OR
              ;; 3. IDs match exactly
              (when (or (not project-id)
                       (not local-id)
                       (string-empty-p local-id)
                       (string= local-id project-id))
                (setq found-pos pos))))))
      found-pos)))

;;; Main Sync Functions ------------------------------------------------------

;;;###autoload
(defun ticktick-fetch-to-org ()
  "Fetch all tasks from TickTick and update org file without duplicating."
  (interactive)
  (ticktick--ensure-backend)
  (if ticktick-multi-file-support
      (ticktick--fetch-to-org-multi)
    (ticktick--fetch-to-org-single)))

(defun ticktick--fetch-to-org-single ()
  "Fetch tasks to single org file."
  (let* ((backend (ticktick--get-backend))
         (projects (ticktick-backend-fetch-projects backend)))
    (with-current-buffer (find-file-noselect ticktick-sync-file)
      (org-with-wide-buffer
       (dolist (project projects)
         (ticktick--sync-project project backend))
       (save-buffer)))))

(defun ticktick--fetch-to-org-multi ()
  "Fetch tasks to multiple org files based on project mapping."
  (let* ((backend (ticktick--get-backend))
         (projects (ticktick-backend-fetch-projects backend))
         (project-file-mapping (ticktick--get-project-file-mapping))
         ;; Determine fallback file for unmapped projects
         (fallback-file (cond
                         ((stringp ticktick-sync-file) ticktick-sync-file)
                         ((and (listp ticktick-sync-file) (car ticktick-sync-file))
                          (car ticktick-sync-file))
                         (t (expand-file-name "inbox.org"
                                              (concat user-emacs-directory "ticktick/"))))))

    (dolist (project projects)
      (let* ((project-id (ticktick-project-id project))
             (project-name (ticktick-project-name project))
             (target-file (or (cdr (assoc project-id project-file-mapping))
                              (cdr (assoc project-name project-file-mapping))
                              fallback-file)))

        (with-current-buffer (find-file-noselect target-file)
          (org-with-wide-buffer
           (let ((project-heading-pos (ticktick--find-project-heading project-name project-id)))
             (if project-heading-pos
                 (progn
                   (goto-char project-heading-pos)
                   (outline-show-subtree)
                   (let ((tasks (ticktick-backend-fetch-tasks backend project-id)))
                     (dolist (task tasks)
                       (ticktick--sync-task task project-heading-pos))))

               (when (or (string= target-file fallback-file)
                         (ticktick--should-project-be-in-file-p project-name target-file))
                 (let ((new-pos (ticktick--create-project-heading project)))
                   (outline-show-subtree)
                   (let ((tasks (ticktick-backend-fetch-tasks backend project-id)))
                     (dolist (task tasks)
                       (ticktick--sync-task task new-pos)))))))
           (save-buffer)))))
    (message "Multi-file synchronization completed")))

;;;###autoload
(defun ticktick-push-from-org ()
  "Push all updated org tasks back to TickTick."
  (interactive)
  (ticktick--ensure-backend)
  (if ticktick-multi-file-support
      (ticktick--push-from-org-multi)
    (ticktick--push-from-org-single)))

(defun ticktick--push-from-org-single ()
  "Push tasks from single org file."
  (let ((backend (ticktick--get-backend))
        (changes '())
        (task-count 0)
        (created-count 0)
        (updated-count 0))
    (with-current-buffer (find-file-noselect ticktick-sync-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (outline-next-heading)
         (when (and (= (org-current-level) 2)
                    (not (org-entry-get nil "TICKTICK_PROJECT_ID")))
           (setq task-count (1+ task-count))
           (let ((title (org-get-heading t t t t)))
             (message "TickTick: Processing task #%d: %s" task-count title))
           (if (ticktick-common-should-sync-p)
               (let* ((task (ticktick-common-org-to-task))
                      (project-id (or (org-entry-get nil "TICKTICK_PROJECT_ID" t)
                                      (ticktick--get-or-ensure-project-id backend)))
                      (id (ticktick-task-id task)))
                 ;; Update task's project-id
                 (setf (ticktick-task-project-id task) project-id)
                 (message "TickTick:   Task ID: %s, Project ID: %s" (or id "none") project-id)
                 (if (and id (not (string-empty-p id)))
                     (progn
                       (message "TickTick:   Updating task...")
                       (ticktick-backend-update-task backend task id project-id)
                       (ticktick-common-update-sync-meta)
                       (setq updated-count (1+ updated-count))
                       (message "TickTick:   ✓ Updated: %s" (ticktick-task-title task)))
                   (message "TickTick:   Creating new task...")
                   (let ((created (ticktick-backend-create-task backend task project-id)))
                     (if created
                         (progn
                           (org-entry-put nil "TICKTICK_ID" (ticktick-task-id created))
                           (org-entry-put nil "TICKTICK_ETAG" (ticktick-task-etag created))
                           (ticktick-common-update-sync-meta)
                           (setq created-count (1+ created-count))
                           (message "TickTick:   ✓ Created: %s (ID: %s)"
                                    (ticktick-task-title created)
                                    (ticktick-task-id created)))
                       (message "TickTick:   ✗ Failed to create task")))))
             (message "TickTick:   Task needs sync: no (skipped)"))))
       (save-buffer)
       (message "TickTick: Push completed - %d tasks found, %d created, %d updated"
                task-count created-count updated-count)))))

(defun ticktick--push-from-org-multi ()
  "Push tasks from multiple org files."
  (let ((backend (ticktick--get-backend))
        (project-files (ticktick--scan-org-files-for-projects)))
    (message "TickTick: Found %d files with projects" (length project-files))
    (dolist (file-info project-files)
      (let ((file-path (car file-info))
            (positions (cdr file-info)))
        (message "TickTick: Processing file %s with %d projects" file-path (length positions))
        (with-current-buffer (find-file-noselect file-path)
          (org-with-wide-buffer
           (dolist (pos positions)
             (save-excursion
               (goto-char pos)
               (let* ((project-name (funcall ticktick-project-name-function))
                      (project-id (ticktick--get-or-create-project-id project-name backend)))
                 (message "TickTick: Project '%s' -> ID: %s" project-name project-id)
                 (when project-id
                   (let ((task-count 0)
                         (created-count 0)
                         (updated-count 0)
                         (skipped-count 0))
                     ;; Ensure point is at project heading for org-map-entries with 'tree scope
                     ;; (ticktick--get-or-create-project-id may have moved point)
                     (save-excursion
                       (goto-char pos)
                       (org-map-entries
                        (lambda ()
                        (let ((level (org-current-level))
                              (title (org-get-heading t t t t))
                              (should-sync (ticktick-common-should-sync-p)))
                          (when (> level 1)
                            (setq task-count (1+ task-count))
                            (message "TickTick:   Task #%d (level %d): %s" task-count level title)
                            (if should-sync
                                (let* ((task (ticktick-common-org-to-task))
                                       (id (ticktick-task-id task)))
                                  ;; Update task's project-id
                                  (setf (ticktick-task-project-id task) project-id)
                                  (message "TickTick:     Task ID: %s, Project ID: %s" (or id "none") project-id)
                                  (if (and id (not (string-empty-p id)))
                                      (progn
                                        (message "TickTick:     Updating...")
                                        (ticktick-backend-update-task backend task id project-id)
                                        (ticktick-common-update-sync-meta)
                                        (setq updated-count (1+ updated-count))
                                        (message "TickTick:     ✓ Updated"))
                                    (message "TickTick:     Creating...")
                                    (let ((created (ticktick-backend-create-task backend task project-id)))
                                      (if created
                                          (progn
                                            (org-entry-put nil "TICKTICK_ID" (ticktick-task-id created))
                                            (org-entry-put nil "TICKTICK_ETAG" (ticktick-task-etag created))
                                            (ticktick-common-update-sync-meta)
                                            (setq created-count (1+ created-count))
                                            (message "TickTick:     ✓ Created (ID: %s)" (ticktick-task-id created)))
                                        (message "TickTick:     ✗ Failed to create")))))
                              (setq skipped-count (1+ skipped-count))
                              (message "TickTick:     Skipped (no changes)")))))
                        nil 'tree))
                     (message "TickTick:   Project summary: %d tasks, %d created, %d updated, %d skipped"
                              task-count created-count updated-count skipped-count))))))
           (save-buffer)))))
    (message "TickTick: Push from org files completed")))

;;;###autoload
(defun ticktick-sync ()
  "Two-way sync: push local changes first, then fetch remote updates."
  (interactive)
  (ticktick--ensure-backend)
  (ticktick-push-from-org)
  (sit-for 1)
  (ticktick-fetch-to-org))

;;; Utility/Admin Commands ---------------------------------------------------

;;;###autoload
(defun ticktick-delete-all-projects ()
  "Delete ALL projects and their tasks from TickTick.

WARNING: This is a DESTRUCTIVE operation that will permanently delete
ALL projects and ALL tasks from TickTick. This action CANNOT be undone!

This function will:
1. Delete all tasks in each project first
2. Then delete the empty project

You will be prompted to type 'yes' in full to confirm this dangerous operation."
  (interactive)
  (ticktick--ensure-backend)
  (let* ((backend (ticktick--get-backend))
         (projects (ticktick-backend-fetch-projects backend))
         (project-count (length projects)))
    (if (= project-count 0)
        (message "No projects found in TickTick")
      (message "Found %d projects in TickTick" project-count)
      (when (yes-or-no-p
             (format "⚠️  WARNING: Delete ALL %d projects and their tasks from TickTick? This CANNOT be undone! "
                     project-count))
        (let ((deleted-projects 0)
              (failed-projects 0)
              (deleted-tasks 0)
              (failed-tasks 0)
              (start-time (current-time)))
          (message "Starting deletion of %d projects..." project-count)
          (dolist (project projects)
            (let ((project-id (ticktick-project-id project))
                  (project-name (ticktick-project-name project)))
              (message "Processing [%d/%d]: %s (ID: %s)"
                       (+ deleted-projects failed-projects 1)
                       project-count
                       project-name
                       project-id)

              ;; Step 1: Delete all tasks in this project
              (condition-case err
                  (let ((tasks (ticktick-backend-fetch-tasks backend project-id)))
                    (when tasks
                      (message "  Deleting %d tasks..." (length tasks))
                      (dolist (task tasks)
                        (condition-case task-err
                            (progn
                              (ticktick-backend-delete-task backend
                                                           (ticktick-task-id task)
                                                           project-id)
                              (setq deleted-tasks (1+ deleted-tasks)))
                          (error
                           (setq failed-tasks (1+ failed-tasks))
                           (message "    ✗ Failed to delete task: %s"
                                   (ticktick-task-title task)))))))
                (error
                 (message "  ✗ Failed to fetch tasks: %S" err)))

              ;; Step 2: Delete the (now empty) project
              (sit-for 0.1)  ; Small delay to avoid rate limiting
              (condition-case err
                  (progn
                    (ticktick-backend-delete-project backend project-id)
                    (setq deleted-projects (1+ deleted-projects))
                    (message "  ✓ Deleted project: %s" project-name))
                (error
                 (setq failed-projects (1+ failed-projects))
                 (message "  ✗ Failed to delete project: %s (Error: %S)"
                         project-name err)))))

          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            (message "Deletion completed in %.1f seconds:\n  Projects: %d deleted, %d failed\n  Tasks: %d deleted, %d failed"
                     elapsed deleted-projects failed-projects deleted-tasks failed-tasks)))))))

;;;###autoload
(defun ticktick-delete-all-tags ()
  "Delete ALL tags from TickTick.

WARNING: This is a DESTRUCTIVE operation that will permanently delete
ALL tags from TickTick. This action CANNOT be undone!

Note: This function only works with V2 backend (Username/Password).

You will be prompted to confirm this dangerous operation."
  (interactive)
  (ticktick--ensure-backend)
  (let ((backend (ticktick--get-backend)))
    ;; Check if backend is V2
    (unless (eq ticktick-backend-type 'v2)
      (user-error "Tag deletion is only supported with V2 backend. Use M-x ticktick-switch-to-v2"))

    (let* ((batch-data (ticktick-v2-request backend "GET" "/batch/check/0"))
           (tags (plist-get batch-data :tags))
           (tag-count (length tags)))
      (if (= tag-count 0)
          (message "No tags found in TickTick")
        (message "Found %d tags in TickTick" tag-count)
        (when (yes-or-no-p
               (format "⚠️  WARNING: Delete ALL %d tags from TickTick? This CANNOT be undone! "
                       tag-count))
          (let ((deleted-tags 0)
                (failed-tags 0)
                (start-time (current-time)))
            (message "Starting deletion of %d tags..." tag-count)
            (dolist (tag tags)
              (let ((tag-name (plist-get tag :name))
                    (tag-label (plist-get tag :label)))
                (message "Deleting [%d/%d]: %s"
                         (+ deleted-tags failed-tags 1)
                         tag-count
                         (or tag-label tag-name))
                (sit-for 0.1)  ; Small delay to avoid rate limiting
                (condition-case err
                    (progn
                      (ticktick-v2-delete-tag backend tag-name)
                      (setq deleted-tags (1+ deleted-tags))
                      (message "  ✓ Deleted tag: %s" (or tag-label tag-name)))
                  (error
                   (setq failed-tags (1+ failed-tags))
                   (message "  ✗ Failed to delete tag: %s (Error: %S)"
                           (or tag-label tag-name) err)))))

            (let ((elapsed (float-time (time-subtract (current-time) start-time))))
              (message "Tag deletion completed in %.1f seconds:\n  Tags: %d deleted, %d failed"
                       elapsed deleted-tags failed-tags))))))))

;;; Backend Switching Commands -----------------------------------------------

;;;###autoload
(defun ticktick-authorize ()
  "Authorize with TickTick using the currently selected backend."
  (interactive)
  (ticktick--ensure-backend)
  (ticktick-backend-authenticate (ticktick--get-backend)))

;;;###autoload
(defun ticktick-refresh-token ()
  "Refresh authentication token (if supported by current backend)."
  (interactive)
  (ticktick--ensure-backend)
  (ticktick-backend-refresh-token (ticktick--get-backend)))

;;;###autoload
(defun ticktick-switch-to-v1 ()
  "Switch to V1 (OAuth2) backend."
  (interactive)
  (setq ticktick-backend-type 'v1)
  (ticktick--init-backend)
  (message "Switched to TickTick V1 (OAuth2) backend"))

;;;###autoload
(defun ticktick-switch-to-v2 ()
  "Switch to V2 (Username/Password) backend."
  (interactive)
  (setq ticktick-backend-type 'v2)
  (ticktick--init-backend)
  (message "Switched to TickTick V2 (Username/Password) backend"))

;;; Project Management -------------------------------------------------------

(defun ticktick--get-project-id ()
  "Get the TickTick project ID from the current org heading."
  (org-entry-get nil "TICKTICK_PROJECT_ID"))

(defun ticktick--project-p-default ()
  "Default function to determine if current heading is a TickTick project.
Returns non-nil if the heading has ORG_GTD property set to \"Projects\"."
  (and (= (org-current-level) 1)
       (string= (org-entry-get nil "ORG_GTD") "Projects")))

(defun ticktick--project-name-default ()
  "Default function to extract project name from current heading."
  (let ((title (org-get-heading t t)))
    (when title
      (string-trim title))))

(defun ticktick--project-p-by-tag ()
  "Alternative project detection function using tags."
  (and (= (org-current-level) 1)
       (member "PROJECT" (org-get-tags))))

(defun ticktick--project-name-from-title ()
  "Alternative function to extract project name from heading title."
  (org-get-heading t t))

(defun ticktick--get-or-create-project-id (project-name backend)
  "Get existing project ID or create new project with PROJECT-NAME using BACKEND."
  (let ((existing-id (org-entry-get nil "TICKTICK_PROJECT_ID")))
    (if (and existing-id (not (string-empty-p existing-id)))
        existing-id
      (let* ((project (ticktick-project-create
                       :name project-name
                       :color "#F18181"
                       :view-mode "list"
                       :kind "TASK"))
             (created (ticktick-backend-create-project backend project)))
        (when created
          (let ((new-id (ticktick-project-id created)))
            (org-entry-put nil "TICKTICK_PROJECT_ID" new-id)
            (org-entry-put nil "TICKTICK_PROJECT_COLOR" (ticktick-project-color created))
            (org-entry-put nil "TICKTICK_PROJECT_VIEWMODE" (ticktick-project-view-mode created))
            (org-entry-put nil "TICKTICK_PROJECT_KIND" (ticktick-project-kind created))
            new-id))))))

(defun ticktick--get-or-ensure-project-id (backend)
  "Get or create project ID for current task.
If task has a parent level 1 heading, use that as project.
If parent doesn't have project ID, create project with parent's name.
If no parent, use Inbox project."
  (save-excursion
    (let ((parent-level-1-pos nil))
      ;; Find parent level 1 heading
      (while (and (org-up-heading-safe)
                  (> (org-current-level) 1)))
      (when (= (org-current-level) 1)
        (setq parent-level-1-pos (point)))

      (if parent-level-1-pos
          ;; Found parent level 1 heading
          (progn
            (goto-char parent-level-1-pos)
            (let ((project-id (org-entry-get nil "TICKTICK_PROJECT_ID")))
              (if (and project-id (not (string-empty-p project-id)))
                  project-id
                ;; No project ID, create one
                (let ((project-name (funcall ticktick-project-name-function)))
                  (ticktick--get-or-create-project-id project-name backend)))))
        ;; No parent, use Inbox
        "inbox"))))

;;;###autoload
(defun ticktick-create-project (name)
  "Interactively create a new TickTick project with NAME."
  (interactive "sProject name: ")
  (ticktick--ensure-backend)
  (let* ((backend (ticktick--get-backend))
         (color (completing-read "Project color (default #F18181): "
                                 '("#F18181" "#7BC96F" "#F9C74F" "#90E0EF" "#C9A0DC" "#FF6B6B" "#4ECDC4" "#45B7D1")
                                 nil t nil nil "#F18181"))
         (view-mode (completing-read "View mode (default list): "
                                     '("list" "kanban" "timeline") nil t nil nil "list"))
         (kind (completing-read "Project kind (default TASK): "
                                '("TASK" "NOTE") nil t nil nil "TASK"))
         (project (ticktick-project-create
                   :name name
                   :color color
                   :view-mode view-mode
                   :kind kind))
         (created (ticktick-backend-create-project backend project)))
    (when created
      (with-current-buffer (find-file-noselect ticktick-sync-file)
        (org-with-wide-buffer
         (ticktick--create-project-heading created)
         (save-buffer))))))

;;;###autoload
(defun ticktick-update-project ()
  "Update the current TickTick project properties."
  (interactive)
  (ticktick--ensure-backend)
  (let* ((project-id (ticktick--get-project-id)))
    (unless project-id
      (user-error "No TickTick project found at current position"))
    (let* ((backend (ticktick--get-backend))
           (current-name (org-entry-get nil "ITEM"))
           (name (read-string (format "Project name (current: %s): " current-name) current-name))
           (color (completing-read "Project color: "
                                   '("#F18181" "#7BC96F" "#F9C74F" "#90E0EF" "#C9A0DC")
                                   nil t))
           (view-mode (completing-read "View mode: "
                                       '("list" "kanban" "timeline") nil t))
           (kind (completing-read "Project kind: "
                                  '("TASK" "NOTE") nil t))
           (project (ticktick-project-create
                     :name name
                     :color color
                     :view-mode view-mode
                     :kind kind))
           (updated (ticktick-backend-update-project backend project project-id)))
      (when updated
        (org-edit-headline name)
        (org-entry-put nil "TICKTICK_PROJECT_COLOR" color)
        (org-entry-put nil "TICKTICK_PROJECT_VIEWMODE" view-mode)
        (org-entry-put nil "TICKTICK_PROJECT_KIND" kind)
        (message "Project updated successfully")))))

;;;###autoload
(defun ticktick-delete-project ()
  "Delete the current TickTick project after confirmation."
  (interactive)
  (ticktick--ensure-backend)
  (let* ((project-id (ticktick--get-project-id))
         (project-name (org-entry-get nil "ITEM")))
    (unless project-id
      (user-error "No TickTick project found at current position"))
    (when (y-or-n-p (format "Are you sure you want to delete project '%s'? " project-name))
      (let ((backend (ticktick--get-backend)))
        (ticktick-backend-delete-project backend project-id)
        (org-mark-subtree)
        (kill-region (region-beginning) (region-end))
        (message "Project '%s' deleted" project-name)))))

;;; Multi-file Support -------------------------------------------------------

(defun ticktick-enable-multi-file ()
  "Enable multi-file synchronization mode."
  (interactive)
  (setq ticktick-multi-file-support t)
  (message "TickTick multi-file support enabled"))

(defun ticktick-disable-multi-file ()
  "Disable multi-file synchronization mode."
  (interactive)
  (setq ticktick-multi-file-support nil)
  (message "TickTick multi-file support disabled"))

(defun ticktick-set-project-detection-by-property ()
  "Set project detection to use ORG_GTD property (default)."
  (interactive)
  (setq ticktick-project-detection-function 'ticktick--project-p-default)
  (setq ticktick-project-name-function 'ticktick--project-name-default)
  (message "Project detection set to ORG_GTD property"))

(defun ticktick-set-project-detection-by-tag ()
  "Set project detection to use PROJECT tag."
  (interactive)
  (setq ticktick-project-detection-function 'ticktick--project-p-by-tag)
  (setq ticktick-project-name-function 'ticktick--project-name-from-title)
  (message "Project detection set to PROJECT tag"))

(defun ticktick--should-project-be-in-file-p (project-name file-path)
  "Determine if PROJECT-NAME should be created in FILE-PATH."
  (with-current-buffer (find-file-noselect file-path)
    (save-excursion
      (goto-char (point-min))
      (let ((found-match nil))
        (while (and (not found-match) (outline-next-heading))
          (when (funcall ticktick-project-detection-function)
            (let ((existing-name (funcall ticktick-project-name-function)))
              (when (string= existing-name project-name)
                (setq found-match t)))))
        found-match))))

(defun ticktick--scan-org-files-for-projects ()
  "Scan configured sources for org files containing project headings.
Sources are checked in this order:
1. If ticktick-sync-file is a list: scan those specific files
2. If ticktick-org-file-directories is set: scan those directories
3. Otherwise: scan currently open org-mode buffers"
  (cond
   ;; Case 1: ticktick-sync-file is a list of files
   ((and ticktick-sync-file (listp ticktick-sync-file))
    (let ((project-files '()))
      (message "TickTick: Scanning files from ticktick-sync-file list (%d files)"
               (length ticktick-sync-file))
      (dolist (file ticktick-sync-file)
        (when (and (file-exists-p file)
                   (not (ticktick--file-already-scanned-p file project-files)))
          (let ((file-projects (ticktick--scan-file-for-projects file)))
            (when file-projects
              (message "TickTick: Found %d projects in %s" (length file-projects) file)
              (push (cons file file-projects) project-files)))))
      (message "TickTick: Total files with projects: %d" (length project-files))
      project-files))

   ;; Case 2: Use directory scanning
   (ticktick-org-file-directories
    (ticktick--scan-directories-for-projects))

   ;; Case 3: Scan open buffers (fallback)
   (t
    (let ((project-files '()))
      (message "TickTick: Scanning open org-mode buffers")
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (eq major-mode 'org-mode)
                     (buffer-file-name)
                     (not (string-match-p "^\\*" (buffer-name))))
            (save-excursion
              (goto-char (point-min))
              (let ((project-positions '()))
                (while (outline-next-heading)
                  (when (funcall ticktick-project-detection-function)
                    (push (point) project-positions)))
                (when project-positions
                  (push (cons (buffer-file-name) (reverse project-positions)) project-files)))))))
      project-files))))

(defun ticktick--get-project-file-mapping ()
  "Create a mapping of TickTick project IDs and names to org file paths.
Returns an alist with both (project-id . file-path) and (project-name . file-path) pairs.
This allows matching by ID when available, or by name as fallback."
  (let ((mapping '()))
    (dolist (file-info (ticktick--scan-org-files-for-projects))
      (let ((file-path (car file-info))
            (positions (cdr file-info)))
        (dolist (pos positions)
          (with-current-buffer (find-file-noselect file-path)
            (save-excursion
              (goto-char pos)
              (let ((project-id (ticktick--get-project-id))
                    (project-name (funcall ticktick-project-name-function)))
                ;; Add ID-based mapping if ID exists and is non-empty
                (when (and project-id (not (string-empty-p project-id)))
                  (push (cons project-id file-path) mapping))
                ;; Always add name-based mapping as fallback
                (when project-name
                  (push (cons project-name file-path) mapping))))))))
    mapping))

(defun ticktick--scan-directories-for-projects ()
  "Scan configured directories for org files containing projects."
  (let ((project-files '()))
    (message "TickTick: Scanning directories: %S" ticktick-org-file-directories)
    (dolist (dir ticktick-org-file-directories)
      (when (file-directory-p dir)
        (message "TickTick: Scanning directory: %s" dir)
        (dolist (pattern ticktick-org-file-patterns)
          (let ((files (ticktick--find-files-recursively dir pattern)))
            (message "TickTick: Found %d files matching '%s' in %s" (length files) pattern dir)
            (dolist (file files)
              (when (and (file-exists-p file)
                         (not (ticktick--file-already-scanned-p file project-files)))
                (let ((file-projects (ticktick--scan-file-for-projects file)))
                  (when file-projects
                    (message "TickTick: Found %d projects in %s" (length file-projects) file)
                    (push (cons file file-projects) project-files)))))))))
    (message "TickTick: Total files with projects: %d" (length project-files))
    project-files))

(defun ticktick--find-files-recursively (dir pattern)
  "Find files matching PATTERN recursively in DIR."
  (let ((files '())
        (pattern-regex (ticktick--wildcard-to-regexp pattern)))
    (ticktick--walk-directory dir
                              (lambda (file)
                                (when (and (string-match-p pattern-regex (file-name-nondirectory file))
                                           (not (file-directory-p file)))
                                  (push file files))))
    (reverse files)))

(defun ticktick--walk-directory (dir callback)
  "Walk directory DIR recursively and call CALLBACK for each file."
  (dolist (file (directory-files dir t "^[^.]"))
    (cond
     ((file-directory-p file)
      (unless (member (file-name-nondirectory file) '("." ".."))
        (ticktick--walk-directory file callback)))
     (t
      (funcall callback file)))))

(defun ticktick--wildcard-to-regexp (wildcard)
  "Convert wildcard pattern to regexp."
  (let ((result (regexp-quote wildcard)))
    (setq result (replace-regexp-in-string "\\\\\\*" ".*" result))
    (setq result (replace-regexp-in-string "\\\\\\?" "." result))
    (concat "^" result "$")))

(defun ticktick--file-already-scanned-p (file project-files)
  "Check if FILE has already been scanned in PROJECT-FILES."
  (assoc file project-files))

(defun ticktick--scan-file-for-projects (file-path)
  "Scan a specific org file for project headings."
  (when (and (file-exists-p file-path)
             (string-match-p "\\.org\\'" file-path))
    (message "TickTick: Scanning file: %s" file-path)
    (with-current-buffer (find-file-noselect file-path)
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (save-excursion
        (goto-char (point-min))
        (let ((project-positions '())
              (heading-count 0))
          (while (outline-next-heading)
            (setq heading-count (1+ heading-count))
            (when (funcall ticktick-project-detection-function)
              (let ((project-name (funcall ticktick-project-name-function)))
                (message "TickTick:   Found project '%s' at position %d" project-name (point))
                (push (point) project-positions))))
          (message "TickTick:   Scanned %d headings, found %d projects" heading-count (length project-positions))
          (reverse project-positions))))))

;;; Autosync and Timer -------------------------------------------------------

(defun ticktick--autosync ()
  "Autosync if enabled."
  (when ticktick-autosync
    (when (file-exists-p ticktick-sync-file)
      (ignore-errors (ticktick-sync)))))

(defun ticktick--setup-sync-timer ()
  "Set up or tear down the sync timer based on `ticktick-sync-interval'."
  (when ticktick--sync-timer
    (cancel-timer ticktick--sync-timer)
    (setq ticktick--sync-timer nil))
  (when (and ticktick-sync-interval
             (numberp ticktick-sync-interval)
             (> ticktick-sync-interval 0))
    (setq ticktick--sync-timer
          (run-at-time ticktick-sync-interval
                       (* ticktick-sync-interval 60)
                       #'ticktick--timer-sync))))

(defun ticktick--timer-sync ()
  "Sync function called by timer."
  (when (file-exists-p ticktick-sync-file)
    (ignore-errors (ticktick-sync))))

;;;###autoload
(defun ticktick-toggle-sync-timer ()
  "Toggle automatic timer-based syncing on/off."
  (interactive)
  (if ticktick--sync-timer
      (progn
        (cancel-timer ticktick--sync-timer)
        (setq ticktick--sync-timer nil)
        (message "TickTick timer sync disabled"))
    (if (and ticktick-sync-interval
             (numberp ticktick-sync-interval)
             (> ticktick-sync-interval 0))
        (progn
          (ticktick--setup-sync-timer)
          (message "TickTick timer sync enabled (every %d minutes)" ticktick-sync-interval))
      (message "Set ticktick-sync-interval to enable timer sync"))))

(defun ticktick--maybe-autosync-on-focus-change (&rest _)
  "Trigger autosync when the selected frame loses focus."
  (when (and (fboundp 'frame-focus-state)
             (not (frame-focus-state (selected-frame))))
    (run-with-idle-timer 0 nil #'ticktick--autosync)))

;;;###autoload
(defun ticktick-enable-autosync-on-blur ()
  "Enable automatic synchronization when Emacs loses window focus."
  (interactive)
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    #'ticktick--maybe-autosync-on-focus-change)
    (with-suppressed-warnings ((obsolete focus-out-hook))
      (add-hook 'focus-out-hook #'ticktick--autosync))))

;;;###autoload
(defun ticktick-disable-autosync-on-blur ()
  "Disable automatic synchronization on window focus loss."
  (interactive)
  (if (boundp 'after-focus-change-function)
      (remove-function after-focus-change-function
                       #'ticktick--maybe-autosync-on-focus-change)
    (with-suppressed-warnings ((obsolete focus-out-hook))
      (remove-hook 'focus-out-hook #'ticktick--autosync))))

(provide 'ticktick)
;;; ticktick.el ends here
