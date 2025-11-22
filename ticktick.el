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
              ;; Explicitly set the etag property after inserting the task
              (when etag
                (org-entry-put nil "TICKTICK_ETAG" etag))
              (ticktick-common-update-sync-meta))))
      (save-excursion
        (goto-char project-pos)
        (outline-next-heading)
        (insert (ticktick-common-task-to-org task) "\n")
        ;; Explicitly set the etag property after inserting the task
        (when etag
          (org-entry-put nil "TICKTICK_ETAG" etag))
        (ticktick-common-update-sync-meta)))))

(defun ticktick--sort-tasks-by-sort-order (project-pos)
  "Sort all tasks under PROJECT-POS by their TICKTICK_SORT_ORDER property.
Only sorts when `ticktick-sync-sort-order' is 'bidirectional."
  (when (eq ticktick-sync-sort-order 'bidirectional)
    (save-excursion
      (goto-char project-pos)
      (let ((project-level (org-current-level))
            (tasks-data '()))
        ;; Collect all tasks under this project with their sort-order
        (org-map-entries
         (lambda ()
           (let* ((level (org-current-level))
                  (has-project-id (org-entry-get nil "TICKTICK_PROJECT_ID"))
                  (sort-order-str (org-entry-get nil "TICKTICK_SORT_ORDER")))
             (when (and (> level project-level)
                        (not has-project-id)
                        sort-order-str)
               (push (cons (string-to-number sort-order-str)
                           (buffer-substring-no-properties
                            (org-entry-beginning-position)
                            (org-entry-end-position)))
                     tasks-data))))
         nil 'tree)
        ;; Sort by sort-order (ascending)
        (setq tasks-data (sort tasks-data (lambda (a b) (< (car a) (car b)))))
        ;; Delete all tasks and re-insert in sorted order
        (when tasks-data
          ;; First, delete all tasks
          (org-map-entries
           (lambda ()
             (let* ((level (org-current-level))
                    (has-project-id (org-entry-get nil "TICKTICK_PROJECT_ID"))
                    (sort-order-str (org-entry-get nil "TICKTICK_SORT_ORDER")))
               (when (and (> level project-level)
                          (not has-project-id)
                          sort-order-str)
                 (delete-region (org-entry-beginning-position)
                                (org-entry-end-position)))))
           nil 'tree)
          ;; Then, insert tasks in sorted order
          (goto-char project-pos)
          (outline-next-heading)
          (dolist (task-data (reverse tasks-data))
            (insert (cdr task-data))))))))

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
        (ticktick--sync-task task project-pos))
      ;; Sort tasks by sort-order if bidirectional mode is enabled
      (ticktick--sort-tasks-by-sort-order project-pos))))

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

        ;; Only sync to files that already have a project heading or are the fallback file
        ;; AND only if the file is in our scan list or is the fallback
        (when (or (string= target-file fallback-file)
                  (member target-file (mapcar #'car (ticktick--scan-org-files-for-projects))))
          (with-current-buffer (find-file-noselect target-file)
            (org-with-wide-buffer
             (let ((project-heading-pos (ticktick--find-project-heading project-name project-id)))
               (if project-heading-pos
                   ;; Project exists - update it
                   (progn
                     (goto-char project-heading-pos)
                     (outline-show-subtree)
                     (let ((tasks (ticktick-backend-fetch-tasks backend project-id)))
                       (dolist (task tasks)
                         (ticktick--sync-task task project-heading-pos))))

                 ;; Project doesn't exist - only create if fallback file
                 (when (string= target-file fallback-file)
                   (let ((new-pos (ticktick--create-project-heading project)))
                     (outline-show-subtree)
                     (let ((tasks (ticktick-backend-fetch-tasks backend project-id)))
                       (dolist (task tasks)
                         (ticktick--sync-task task new-pos))))))))
           (save-buffer)))))
    (message "Multi-file synchronization completed")))

;;;###autoload
(defun ticktick-push-from-org ()
  "Push all updated org tasks back to TickTick."
  (interactive)
  (message "DEBUG: ticktick-push-from-org called, multi-file=%s" ticktick-multi-file-support)
  (ticktick--ensure-backend)
  (if ticktick-multi-file-support
      (progn
        (message "DEBUG: Calling ticktick--push-from-org-multi")
        (ticktick--push-from-org-multi))
    (progn
      (message "DEBUG: Calling ticktick--push-from-org-single")
      (ticktick--push-from-org-single))))

(defun ticktick--push-from-org-single ()
  "Push tasks from single org file."
  (let ((backend (ticktick--get-backend))
        (changes '())
        (task-count 0)
        (created-count 0)
        (updated-count 0)
        (sort-order-counter 0))
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
                 ;; Assign sort-order based on org heading position if enabled
                 ;; TickTick uses negative values where smaller (more negative) = higher position
                 (when (memq ticktick-sync-sort-order '(push-only bidirectional))
                   (setf (ticktick-task-sort-order task) (- -1000000 sort-order-counter))
                   (setq sort-order-counter (1+ sort-order-counter)))
                 (message "TickTick:   Task ID: %s, Project ID: %s" (or id "none") project-id)
                 (if (and id (not (string-empty-p id)))
                     (progn
                       (message "TickTick:   Updating task...")
                       (let ((updated (ticktick-backend-update-task backend task id project-id)))
                         (when updated
                           (when (ticktick-task-etag updated)
                             (org-entry-put nil "TICKTICK_ETAG" (ticktick-task-etag updated)))
                           (when (ticktick-task-sort-order updated)
                             (org-entry-put nil "TICKTICK_SORT_ORDER"
                                            (number-to-string (ticktick-task-sort-order updated))))))
                       (ticktick-common-update-sync-meta)
                       (setq updated-count (1+ updated-count))
                       (message "TickTick:   ✓ Updated: %s" (ticktick-task-title task)))
                   (message "TickTick:   Creating new task...")
                   (let ((created (ticktick-backend-create-task backend task project-id)))
                     (if created
                         (progn
                           (org-entry-put nil "TICKTICK_ID" (ticktick-task-id created))
                           (org-entry-put nil "TICKTICK_ETAG" (ticktick-task-etag created))
                           (when (ticktick-task-sort-order created)
                             (org-entry-put nil "TICKTICK_SORT_ORDER"
                                            (number-to-string (ticktick-task-sort-order created))))
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
  (message "DEBUG: ticktick--push-from-org-multi CALLED")
  (let ((backend (ticktick--get-backend))
        (project-files (ticktick--scan-org-files-for-projects)))
    (message "DEBUG: project-files = %S" project-files)
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
                         (skipped-count 0)
                         (sort-order-counter 0))
                     ;; Process all subtasks under this project
                     ;; Skip the project heading itself and only process descendants
                     (save-excursion
                       (goto-char pos)
                       (let ((project-level (org-current-level))
                             (end-of-project (save-excursion
                                               (goto-char pos)
                                               (org-end-of-subtree t t))))
                         ;; Use org-map-entries with a filter to skip the project itself
                         (org-map-entries
                          (lambda ()
                            (let* ((level (org-current-level))
                                   (title (org-get-heading t t t t))
                                   (has-project-id (org-entry-get nil "TICKTICK_PROJECT_ID"))
                                   (should-sync (ticktick-common-should-sync-p)))
                              ;; Only process if:
                              ;; 1. Level is deeper than project level (not the project itself)
                              ;; 2. Does NOT have TICKTICK_PROJECT_ID (not a nested project)
                              (when (and (> level project-level)
                                         (not has-project-id))
                                (setq task-count (1+ task-count))
                                (message "TickTick:   Task #%d (level %d): %s" task-count level title)
                                (if should-sync
                                    (let* ((task (ticktick-common-org-to-task))
                                           (id (ticktick-task-id task)))
                                      ;; Update task's project-id
                                      (setf (ticktick-task-project-id task) project-id)
                                      ;; Assign sort-order based on org heading position if enabled
                                      ;; TickTick uses negative values where smaller (more negative) = higher position
                                      (when (memq ticktick-sync-sort-order '(push-only bidirectional))
                                        (setf (ticktick-task-sort-order task) (- -1000000 sort-order-counter))
                                        (setq sort-order-counter (1+ sort-order-counter)))
                                      (message "TickTick:     Task ID: %s, Project ID: %s" (or id "none") project-id)
                                      (if (and id (not (string-empty-p id)))
                                          (progn
                                            (message "TickTick:     Updating...")
                                            (let ((updated (ticktick-backend-update-task backend task id project-id)))
                                              (when updated
                                                (when (ticktick-task-etag updated)
                                                  (org-entry-put nil "TICKTICK_ETAG" (ticktick-task-etag updated)))
                                                (when (ticktick-task-sort-order updated)
                                                  (org-entry-put nil "TICKTICK_SORT_ORDER"
                                                                 (number-to-string (ticktick-task-sort-order updated))))))
                                            (ticktick-common-update-sync-meta)
                                            (setq updated-count (1+ updated-count))
                                            (message "TickTick:     ✓ Updated"))
                                        (message "TickTick:     Creating...")
                                        (let ((created (ticktick-backend-create-task backend task project-id)))
                                          (if created
                                              (progn
                                                (org-entry-put nil "TICKTICK_ID" (ticktick-task-id created))
                                                (org-entry-put nil "TICKTICK_ETAG" (ticktick-task-etag created))
                                                (when (ticktick-task-sort-order created)
                                                  (org-entry-put nil "TICKTICK_SORT_ORDER"
                                                                 (number-to-string (ticktick-task-sort-order created))))
                                                (ticktick-common-update-sync-meta)
                                                (setq created-count (1+ created-count))
                                                (message "TickTick:     ✓ Created (ID: %s)" (ticktick-task-id created)))
                                            (message "TickTick:     ✗ Failed to create")))))
                                  (setq skipped-count (1+ skipped-count))
                                  (message "TickTick:     Skipped (no changes)")))))
                          nil 'tree)))
                     (message "TickTick:   Project summary: %d tasks, %d created, %d updated, %d skipped"
                              task-count created-count updated-count skipped-count))))))
           (save-buffer)))))
    (message "TickTick: Push from org files completed")))

;;;###autoload
(defun ticktick-sync ()
  "Two-way sync: push local changes first, then fetch remote updates."
  (interactive)
  (message "DEBUG: === ticktick-sync CALLED ===")
  (message "DEBUG: ticktick-multi-file-support = %s" ticktick-multi-file-support)
  (ticktick--ensure-backend)
  (message "DEBUG: Calling ticktick-push-from-org...")
  (ticktick-push-from-org)
  (message "DEBUG: Calling sit-for...")
  (sit-for 1)
  (message "DEBUG: Calling ticktick-fetch-to-org...")
  (ticktick-fetch-to-org)
  (message "DEBUG: === ticktick-sync DONE ==="))

;;; Utility/Admin Commands ---------------------------------------------------

;;;###autoload
(defun ticktick-clear-sync-cache ()
  "Clear SYNC_CACHE property from all TickTick tasks in current buffer.
This will force all tasks to be re-synchronized on next sync, which is useful when:
- Status keywords have changed (e.g., DONE -> KILL)
- You want to force update all tasks regardless of changes
- Fixing sync issues"
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))

  (save-excursion
    (let ((cleared-count 0))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:SYNC_CACHE:" nil t)
        (save-excursion
          (org-back-to-heading t)
          (org-entry-delete nil "SYNC_CACHE")
          (setq cleared-count (1+ cleared-count))))
      (save-buffer)
      (message "Cleared SYNC_CACHE from %d tasks. Run sync to update TickTick." cleared-count))))

;;;###autoload
(defun ticktick-clear-sync-cache-current-project ()
  "Clear SYNC_CACHE from all tasks under current project heading.
Useful for forcing re-sync of a specific project."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))

  (save-excursion
    (org-back-to-heading t)
    ;; Navigate to project heading (level 1 with TICKTICK_PROJECT_ID)
    (while (and (org-up-heading-safe)
                (not (org-entry-get nil "TICKTICK_PROJECT_ID"))))

    (unless (org-entry-get nil "TICKTICK_PROJECT_ID")
      (user-error "Not inside a TickTick project"))

    (let ((project-name (org-get-heading t t t t))
          (project-start (point))
          (cleared-count 0))
      (message "Clearing SYNC_CACHE in project '%s'..." project-name)
      (org-map-entries
       (lambda ()
         (let ((heading (org-get-heading t t t t)))
           (when (org-entry-get nil "SYNC_CACHE")
             (message "  Clearing SYNC_CACHE from: %s" heading)
             (org-entry-delete nil "SYNC_CACHE")
             (setq cleared-count (1+ cleared-count)))))
       nil 'tree)
      (save-buffer)
      (message "Cleared SYNC_CACHE from %d tasks in project '%s'. Run sync to update TickTick."
               cleared-count project-name))))

;;;###autoload
(defun ticktick-force-sync-current-file ()
  "Clear all SYNC_CACHE in current file and immediately sync.
This is a convenience function that combines cache clearing and syncing."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))

  (when (yes-or-no-p "Force re-sync all tasks in this file? This will update all tasks in TickTick. ")
    (ticktick-clear-sync-cache)
    (sit-for 0.5)  ; Give user time to see the message
    (message "Starting sync...")
    (ticktick-sync-current-file)))

;;;###autoload
(defun ticktick-force-sync-current-project ()
  "Clear SYNC_CACHE in current project and immediately sync.
This is a convenience function for re-syncing a single project."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))

  (ticktick-clear-sync-cache-current-project)
  (sit-for 0.5)  ; Give user time to see the message
  (message "Starting sync...")
  (ticktick-sync-current-file))

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

;;; Current File/Project Sync Functions -----------------------------------------

;;;###autoload
(defun ticktick-sync-current-file ()
  "Sync the current org file with TickTick.
This function will:
1. Check if current file is a valid org file
2. Push local changes to TickTick
3. Fetch remote updates from TickTick
4. Update only the current file"
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))
  
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  
  (ticktick--ensure-backend)
  (let ((file-path (buffer-file-name)))
    (message "TickTick: Syncing current file: %s" (file-name-nondirectory file-path))
    
    ;; Step 1: Push local changes
    (ticktick--push-from-org-single-file file-path)
    
    ;; Step 2: Fetch remote updates
    (sit-for 1)  ; Brief pause to avoid conflicts
    (ticktick--fetch-to-org-single-file file-path)
    
    (message "TickTick: Current file sync completed")))

;;;###autoload
(defun ticktick-sync-current-project ()
  "Sync the current project with TickTick.
This function will:
1. Find the project that contains the current cursor position
2. Push local changes for this project only
3. Fetch remote updates for this project only
4. Update only tasks within this project"
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))
  
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  
  ;; Find the project containing current position
  (save-excursion
    (org-back-to-heading t)
    (let ((current-pos (point))
          (project-pos nil)
          (project-name nil)
          (project-id nil))
      
      ;; Find parent project (level 1 heading with project detection)
      (while (and (org-up-heading-safe)
                  (> (org-current-level) 1)))
      
      (when (= (org-current-level) 1)
        (setq project-pos (point))
        (setq project-name (funcall ticktick-project-name-function))
        (setq project-id (org-entry-get nil "TICKTICK_PROJECT_ID")))
      
      (unless project-pos
        (user-error "No project found at current position"))
      
      (unless (funcall ticktick-project-detection-function)
        (user-error "Current heading is not detected as a TickTick project"))
      
      (ticktick--ensure-backend)
      (let ((backend (ticktick--get-backend)))
        (message "TickTick: Syncing project: %s" project-name)
        
        ;; Ensure project exists on server
        (unless (and project-id (not (string-empty-p project-id)))
          (setq project-id (ticktick--get-or-create-project-id project-name backend))
          (when project-id
            (org-entry-put nil "TICKTICK_PROJECT_ID" project-id)))
        
        (when project-id
          ;; Step 1: Push local changes for this project
          (ticktick--push-from-org-single-project backend project-pos project-id)
          
          ;; Step 2: Fetch remote updates for this project
          (sit-for 1)  ; Brief pause to avoid conflicts
          (ticktick--fetch-to-org-single-project backend project-pos project-id)
          
          (message "TickTick: Project sync completed: %s" project-name))))))

;;;###autoload
(defun ticktick-fetch-current-project-to-org ()
  "Fetch tasks for the current project from TickTick to org.
This only fetches updates (pull direction) and does not push local changes."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))
  
  ;; Find the project containing current position
  (save-excursion
    (org-back-to-heading t)
    (let ((project-pos nil)
          (project-name nil)
          (project-id nil))
      
      ;; Find parent project (level 1 heading with project detection)
      (while (and (org-up-heading-safe)
                  (> (org-current-level) 1)))
      
      (when (= (org-current-level) 1)
        (setq project-pos (point))
        (setq project-name (funcall ticktick-project-name-function))
        (setq project-id (org-entry-get nil "TICKTICK_PROJECT_ID")))
      
      (unless project-pos
        (user-error "No project found at current position"))
      
      (unless (funcall ticktick-project-detection-function)
        (user-error "Current heading is not detected as a TickTick project"))
      
      (ticktick--ensure-backend)
      (let ((backend (ticktick--get-backend)))
        (message "TickTick: Fetching project: %s" project-name)
        
        ;; Ensure project exists on server
        (unless (and project-id (not (string-empty-p project-id)))
          (setq project-id (ticktick--get-or-create-project-id project-name backend))
          (when project-id
            (org-entry-put nil "TICKTICK_PROJECT_ID" project-id)))
        
        (when project-id
          (ticktick--fetch-to-org-single-project backend project-pos project-id)
          (message "TickTick: Project fetch completed: %s" project-name))))))

;;;###autoload
(defun ticktick-push-current-project-from-org ()
  "Push tasks for the current project from org to TickTick.
This only pushes local changes (push direction) and does not fetch remote updates."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))
  
  ;; Find the project containing current position
  (save-excursion
    (org-back-to-heading t)
    (let ((project-pos nil)
          (project-name nil)
          (project-id nil))
      
      ;; Find parent project (level 1 heading with project detection)
      (while (and (org-up-heading-safe)
                  (> (org-current-level) 1)))
      
      (when (= (org-current-level) 1)
        (setq project-pos (point))
        (setq project-name (funcall ticktick-project-name-function))
        (setq project-id (org-entry-get nil "TICKTICK_PROJECT_ID")))
      
      (unless project-pos
        (user-error "No project found at current position"))
      
      (unless (funcall ticktick-project-detection-function)
        (user-error "Current heading is not detected as a TickTick project"))
      
      (ticktick--ensure-backend)
      (let ((backend (ticktick--get-backend)))
        (message "TickTick: Pushing project: %s" project-name)
        
        ;; Ensure project exists on server
        (unless (and project-id (not (string-empty-p project-id)))
          (setq project-id (ticktick--get-or-create-project-id project-name backend))
          (when project-id
            (org-entry-put nil "TICKTICK_PROJECT_ID" project-id)))
        
        (when project-id
          (ticktick--push-from-org-single-project backend project-pos project-id)
          (message "TickTick: Project push completed: %s" project-name))))))

;;; Helper Functions for Current File/Project Sync ------------------------------

(defun ticktick--push-from-org-single-file (file-path)
  "Push changes from a single org FILE-PATH to TickTick."
  (let ((backend (ticktick--get-backend))
        (task-count 0)
        (created-count 0)
        (updated-count 0))
    (with-current-buffer (find-file-noselect file-path)
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
                                      (ticktick--get-or-ensure-project-id backend))))
                 ;; Update task's project-id
                 (setf (ticktick-task-project-id task) project-id)
                 (message "TickTick:   Task ID: %s, Project ID: %s" 
                          (or (ticktick-task-id task) "none") project-id)
                 (let ((id (ticktick-task-id task)))
                   (if (and id (not (string-empty-p id)))
                       (progn
                         (message "TickTick:   Updating task...")
                         (let ((updated (ticktick-backend-update-task backend task id project-id)))
                           (when updated
                             (when (ticktick-task-etag updated)
                               (org-entry-put nil "TICKTICK_ETAG" (ticktick-task-etag updated)))))
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
                         (message "TickTick:   ✗ Failed to create task"))))))
             (message "TickTick:   Task needs sync: no (skipped)"))))
       (save-buffer)
       (message "TickTick: Push completed - %d tasks found, %d created, %d updated"
                task-count created-count updated-count))))

(defun ticktick--fetch-to-org-single-file (file-path)
  "Fetch tasks from TickTick to a single org FILE-PATH."
  (let* ((backend (ticktick--get-backend))
         (projects (ticktick-backend-fetch-projects backend)))
    (with-current-buffer (find-file-noselect file-path)
      (org-with-wide-buffer
       (dolist (project projects)
(let* ((project-id (ticktick-project-id project))
                 (project-title (ticktick-project-name project))
                 (project-heading-re (format "^\\* %s$" (regexp-quote project-title)))
                 (project-pos (save-excursion
                                (goto-char (point-min))
                                (when (re-search-forward project-heading-re nil t)
                                  (match-beginning 0)))))
            (when project-pos
              (goto-char project-pos)
              (outline-show-subtree)
              (let ((tasks (ticktick-backend-fetch-tasks backend project-id)))
                (dolist (task tasks)
                  (ticktick--sync-task task project-pos))
                ;; Sort tasks by sort-order if bidirectional mode is enabled
                (ticktick--sort-tasks-by-sort-order project-pos)))))
       (save-buffer)))))

(defun ticktick--push-from-org-single-project (backend project-pos project-id)
  "Push changes from a single project at PROJECT-POS with PROJECT-ID."
  (save-excursion
    (goto-char project-pos)
    (let ((project-level (org-current-level))
          (task-count 0)
          (created-count 0)
          (updated-count 0)
          (skipped-count 0)
          (sort-order-counter 0))

      ;; Process all subtasks under this project
      (save-excursion
        (let ((end-of-project (save-excursion
                              (goto-char project-pos)
                              (org-end-of-subtree t t))))
          (goto-char project-pos)
          (org-map-entries
           (lambda ()
             (let* ((level (org-current-level))
                    (title (org-get-heading t t t t))
                    (has-project-id (org-entry-get nil "TICKTICK_PROJECT_ID"))
                    (should-sync (ticktick-common-should-sync-p)))
               ;; Only process if:
               ;; 1. Level is deeper than project level (not the project itself)
               ;; 2. Does NOT have TICKTICK_PROJECT_ID (not a nested project)
               (when (and (> level project-level)
                          (not has-project-id))
                 (setq task-count (1+ task-count))
                 (message "TickTick:   Task #%d (level %d): %s" task-count level title)
                 (if should-sync
                     (let* ((task (ticktick-common-org-to-task))
                            (id (ticktick-task-id task)))
                       ;; Update task's project-id
                       (setf (ticktick-task-project-id task) project-id)
                       ;; Assign sort-order based on org heading position if enabled
                       ;; TickTick uses negative values where smaller (more negative) = higher position
                       (when (memq ticktick-sync-sort-order '(push-only bidirectional))
                         (setf (ticktick-task-sort-order task) (- -1000000 sort-order-counter))
                         (setq sort-order-counter (1+ sort-order-counter)))
                       (message "TickTick:     Task ID: %s, Project ID: %s" 
                                (or id "none") project-id)
                       (if (and id (not (string-empty-p id)))
                           (progn
                             (message "TickTick:     Updating...")
                             (let ((updated (ticktick-backend-update-task backend task id project-id)))
                               (when updated
                                 (when (ticktick-task-etag updated)
                                   (org-entry-put nil "TICKTICK_ETAG" (ticktick-task-etag updated)))
                                 (when (ticktick-task-sort-order updated)
                                   (org-entry-put nil "TICKTICK_SORT_ORDER"
                                                  (number-to-string (ticktick-task-sort-order updated))))))
                             (ticktick-common-update-sync-meta)
                             (setq updated-count (1+ updated-count))
                             (message "TickTick:     ✓ Updated"))
                         (message "TickTick:     Creating...")
                         (let ((created (ticktick-backend-create-task backend task project-id)))
                           (if created
                               (progn
                                 (org-entry-put nil "TICKTICK_ID" (ticktick-task-id created))
                                 (org-entry-put nil "TICKTICK_ETAG" (ticktick-task-etag created))
                                 (when (ticktick-task-sort-order created)
                                   (org-entry-put nil "TICKTICK_SORT_ORDER"
                                                  (number-to-string (ticktick-task-sort-order created))))
                                 (ticktick-common-update-sync-meta)
                                 (setq created-count (1+ created-count))
                                 (message "TickTick:     ✓ Created (ID: %s)" (ticktick-task-id created)))
                             (message "TickTick:     ✗ Failed to create")))))
                   (setq skipped-count (1+ skipped-count))
                   (message "TickTick:     Skipped (no changes)")))))
           nil 'tree)))
      
      (message "TickTick:   Project summary: %d tasks, %d created, %d updated, %d skipped"
               task-count created-count updated-count skipped-count)))))

(defun ticktick--fetch-to-org-single-project (backend project-pos project-id)
  "Fetch tasks for a single project at PROJECT-POS with PROJECT-ID."
  (save-excursion
    (goto-char project-pos)
    (outline-show-subtree)
    (let ((tasks (ticktick-backend-fetch-tasks backend project-id)))
      (dolist (task tasks)
        (ticktick--sync-task task project-pos))
      ;; Sort tasks by sort-order if bidirectional mode is enabled
      (ticktick--sort-tasks-by-sort-order project-pos))))

;;; Task Ordering Functions -----------------------------------------------

;;;###autoload
(defun ticktick-set-task-sort-order ()
  "Set sort order for current task.
This allows manual control of task ordering in TickTick V2.
The sort order is an integer where lower numbers appear higher in the list."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))
  
  (unless (org-entry-get nil "TICKTICK_ID")
    (user-error "No TICKTICK_ID found at current heading"))
  
  (let* ((current-sort-order (org-entry-get nil "TICKTICK_SORT_ORDER"))
         (new-order (read-number (format "Sort order (current: %s): " 
                                        (or current-sort-order "not set")))))
    (org-entry-put nil "TICKTICK_SORT_ORDER" (number-to-string new-order))
    (message "Task sort order set to: %d" new-order)))

;;;###autoload
(defun ticktick-move-task-up ()
  "Move current task up in sort order.
Decreases the sort order value by 1."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))
  
  (unless (org-entry-get nil "TICKTICK_ID")
    (user-error "No TICKTICK_ID found at current heading"))
  
  (let* ((current-sort-order (org-entry-get nil "TICKTICK_SORT_ORDER"))
         (current-value (if current-sort-order (string-to-number current-sort-order) 0)))
    (if (> current-value 0)
        (progn
          (org-entry-put nil "TICKTICK_SORT_ORDER" (number-to-string (1- current-value)))
          (message "Task moved up - new sort order: %d" (1- current-value)))
      (message "Task is already at the top (sort order: %d)" current-value))))

;;;###autoload
(defun ticktick-move-task-down ()
  "Move current task down in sort order.
Increases the sort order value by 1."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))
  
  (unless (org-entry-get nil "TICKTICK_ID")
    (user-error "No TICKTICK_ID found at current heading"))
  
  (let* ((current-sort-order (org-entry-get nil "TICKTICK_SORT_ORDER"))
         (current-value (if current-sort-order (string-to-number current-sort-order) 0)))
    (org-entry-put nil "TICKTICK_SORT_ORDER" (number-to-string (1+ current-value)))
    (message "Task moved down - new sort order: %d" (1+ current-value))))

;;;###autoload
(defun ticktick-reset-project-sort-order ()
  "Reset sort order for all tasks in current project.
Sets sequential sort order starting from 0 based on current org order."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Current buffer is not in org-mode"))
  
  ;; Find project containing current position
  (save-excursion
    (org-back-to-heading t)
    (let ((project-pos nil)
          (project-level nil))
      
      ;; Find parent project (level 1 heading with project detection)
      (while (and (org-up-heading-safe)
                  (> (org-current-level) 1)))
      
      (when (= (org-current-level) 1)
        (setq project-pos (point))
        (setq project-level (org-current-level)))
      
      (unless (and project-pos (funcall ticktick-project-detection-function))
        (user-error "No project found at current position"))
      
      (message "Resetting sort order for project...")
      (let ((task-count 0))
        (save-excursion
          (goto-char project-pos)
          (let ((end-of-project (save-excursion
                                (goto-char project-pos)
                                (org-end-of-subtree t t))))
            (goto-char project-pos)
            (org-map-entries
             (lambda ()
               (let* ((level (org-current-level))
                      (has-project-id (org-entry-get nil "TICKTICK_PROJECT_ID")))
                 ;; Only process tasks (not nested projects)
                 (when (and (> level project-level)
                            (not has-project-id)
                            (org-entry-get nil "TICKTICK_ID"))
                   (setq task-count (1+ task-count))
                   (org-entry-put nil "TICKTICK_SORT_ORDER" (number-to-string task-count))
                   (message "  Task %d: sort order set to %d" task-count task-count))))
             nil 'tree)))
        (message "Sort order reset for %d tasks in project" task-count)))))

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
