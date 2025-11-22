;;; ticktick-common.el --- Common data structures for TickTick -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Paul Huang
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: tools, ticktick, data
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
;; This file defines common data structures and conversion functions
;; used by all TickTick backends. It provides a backend-independent
;; representation of tasks and projects, and handles conversion between
;; org-mode and internal formats.

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'subr-x)

;;; Customization Variables --------------------------------------------------

(defcustom ticktick-sync-sort-order 'push-only
  "How to sync task order between org-mode and TickTick.
- 'push-only: Assign sort-order based on org heading order when pushing to TickTick
- 'bidirectional: Also reorder org headings based on TickTick sort-order when fetching
- nil: Don't sync sort-order automatically (manual management only)"
  :type '(choice (const :tag "Push org order to TickTick" push-only)
                 (const :tag "Sync both ways" bidirectional)
                 (const :tag "Manual only" nil))
  :group 'ticktick)

(defcustom ticktick-cancelled-keywords '("CANCELLED" "KILL" "CNCL")
  "List of org TODO keywords that map to TickTick's cancelled status.
These keywords will be synced as 'won\\'t do' (계획취소) status in TickTick."
  :type '(repeat string)
  :group 'ticktick)

;;; Internal Data Structures -------------------------------------------------

(cl-defstruct (ticktick-task
               (:constructor ticktick-task-create)
               (:copier ticktick-task-copy))
  "Internal representation of a TickTick task."
  id           ; Task ID (string)
  title        ; Task title (string)
  status       ; Status symbol: active, completed, cancelled
  priority     ; Priority (0=none, 1=low, 3=medium, 5=high)
  due-date     ; Due date (datetime string or nil)
  content      ; Task description/content (string or nil)
  etag         ; ETag for conflict detection (string or nil)
  project-id   ; Parent project ID (string)
  sort-order   ; Sort order (integer or nil)
  tags         ; List of tag strings
  kind         ; Task kind (V2 only): TEXT, NOTE, CHECKLIST
  created-time ; Creation timestamp (datetime string or nil)
  modified-time ; Last modification timestamp (datetime string or nil)
  completed-time ; Completion timestamp (datetime string or nil)
  )

(cl-defstruct (ticktick-project
               (:constructor ticktick-project-create)
               (:copier ticktick-project-copy))
  "Internal representation of a TickTick project."
  id           ; Project ID (string)
  name         ; Project name (string)
  color        ; Color hex code (string)
  view-mode    ; View mode: list, kanban, timeline
  kind         ; Project kind: TASK, NOTE
  )

;;; Status Constants and Conversion ------------------------------------------

(defconst ticktick-status-active 'active
  "Internal status for active/todo tasks.")

(defconst ticktick-status-completed 'completed
  "Internal status for completed/done tasks.")

(defconst ticktick-status-cancelled 'cancelled
  "Internal status for cancelled/won't-do tasks (V2 only).")

(defun ticktick-common-org-status-to-internal (todo-type todo-keyword)
  "Convert org TODO-TYPE and TODO-KEYWORD to internal status symbol.
TODO-TYPE is 'todo or 'done from `org-element-property'.
TODO-KEYWORD is the actual keyword string like 'TODO', 'DONE', 'CANCELLED', 'KILL', etc."
  (cond
   ((eq todo-type 'done)
    (if (and todo-keyword (member todo-keyword ticktick-cancelled-keywords))
        ticktick-status-cancelled
      ticktick-status-completed))
   (t ticktick-status-active)))

(defun ticktick-common-internal-status-to-org (status)
  "Convert internal STATUS symbol to org TODO keyword string.
Returns one of: 'TODO', 'DONE', 'CANCELLED'."
  (pcase status
    ('active "TODO")
    ('completed "DONE")
    ('cancelled "CANCELLED")
    (_ "TODO")))

;;; Priority Conversion ------------------------------------------------------

(defun ticktick-common-priority-to-org (priority)
  "Convert numeric PRIORITY (0/1/3/5) to org priority character (A/B/C).
Returns priority character (?A, ?B, ?C) or nil."
  (pcase priority
    (5 ?A)  ; High -> A
    (3 ?B)  ; Medium -> B
    (1 ?C)  ; Low -> C
    (_ nil)))

(defun ticktick-common-org-priority-to-number (priority-char)
  "Convert org PRIORITY-CHAR (?A/?B/?C) to numeric value (5/3/1).
Returns numeric priority or 0 for no priority."
  (pcase priority-char
    (?A 5)  ; A -> High
    (?B 3)  ; B -> Medium
    (?C 1)  ; C -> Low
    (_ 0)))

;;; Org <-> Internal Task Conversion -----------------------------------------

(defun ticktick-common-org-to-task ()
  "Convert org heading at point to internal task structure.
Returns a `ticktick-task' struct."
  (let* ((el (org-element-at-point))
         (title (org-element-property :raw-value el))
         (todo-type (org-element-property :todo-type el))
         (todo-keyword (org-element-property :todo-keyword el))
         (priority-char (org-element-property :priority el))
         (deadline (org-element-property :deadline el))
         (closed (org-element-property :closed el))
         (id (org-entry-get nil "TICKTICK_ID"))
         (etag (org-entry-get nil "TICKTICK_ETAG"))
         (project-id (org-entry-get nil "TICKTICK_PROJECT_ID" t))
         (sort-order (org-entry-get nil "TICKTICK_SORT_ORDER"))
         (tags (org-get-tags))
         (content (ticktick-common--extract-content))
         ;; Extract TODO keyword directly from heading text as fallback
         (heading-text (save-excursion (org-back-to-heading t) (org-get-heading t)))
         (extracted-keyword (when (string-match "^\\(TODO\\|DONE\\|NEXT\\|WAITING\\|HOLD\\|CANCELLED\\|KILL\\|CNCL\\|SOMEDAY\\) " heading-text)
                              (match-string 1 heading-text)))
         ;; Use extracted keyword if org-element didn't recognize it
         (final-keyword (or todo-keyword extracted-keyword))
         ;; Determine type: if keyword is in cancelled list or DONE, it's done type
         (final-type (cond
                      ((member final-keyword ticktick-cancelled-keywords) 'done)
                      ((member final-keyword '("DONE")) 'done)
                      (todo-type todo-type)
                      (final-keyword 'todo)
                      (t nil)))
         (internal-status (ticktick-common-org-status-to-internal final-type final-keyword)))
    (ticktick-task-create
     :id id
     :title title
     :status internal-status
     :priority (ticktick-common-org-priority-to-number priority-char)
     :due-date (when deadline
                 (format-time-string "%Y-%m-%dT%H:%M:%S+0000"
                                     (org-timestamp-to-time deadline)))
     :content content
     :etag etag
     :project-id project-id
     :sort-order (when sort-order (string-to-number sort-order))
     :tags tags
     :kind "TEXT"  ; Default to TEXT, backends may override
     :created-time nil
     :modified-time nil
     :completed-time (when closed
                       (format-time-string "%Y-%m-%dT%H:%M:%S+0000"
                                           (org-timestamp-to-time closed))))))

(defun ticktick-common--extract-content ()
  "Extract content/description from current org subtree.
Returns string or nil."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (forward-line)
      ;; Skip planning line
      (while (looking-at org-planning-line-re)
        (forward-line))
      ;; Skip property drawer
      (when (looking-at ":PROPERTIES:")
        (re-search-forward "^:END:" nil t)
        (forward-line))
      (let ((content (buffer-substring-no-properties (point) (point-max))))
        (if (string-empty-p (string-trim content))
            nil
          (string-trim content))))))

(defun ticktick-common-task-to-org (task)
  "Convert internal TASK structure to org heading string.
Returns a formatted org heading with properties."
  (let ((id (ticktick-task-id task))
        (title (ticktick-task-title task))
        (status (ticktick-task-status task))
        (priority (ticktick-task-priority task))
        (due-date (ticktick-task-due-date task))
        (etag (ticktick-task-etag task))
        (content (ticktick-task-content task))
        (sort-order (ticktick-task-sort-order task)))
    (string-join
     (delq nil
           (list
            ;; Heading line with status, priority, and title
            (format "** %s%s %s"
                    (ticktick-common-internal-status-to-org status)
                    (let ((p (ticktick-common-priority-to-org priority)))
                      (if p (format " [#%c]" p) ""))
                    title)
            ;; Deadline
            (when due-date
              (condition-case nil
                  (format "DEADLINE: <%s>"
                          (format-time-string "%F %a" (date-to-time due-date)))
                (error nil)))
            ;; Properties drawer
            ":PROPERTIES:"
            (when id (format ":TICKTICK_ID: %s" id))
            (when etag (format ":TICKTICK_ETAG: %s" etag))
            (when sort-order (format ":TICKTICK_SORT_ORDER: %s" sort-order))
            ":END:"
            ;; Content
            (when content (string-trim content))))
     "\n")))

;;; Org <-> Internal Project Conversion --------------------------------------

(defun ticktick-common-org-to-project ()
  "Convert org heading at point to internal project structure.
Returns a `ticktick-project' struct."
  (let* ((el (org-element-at-point))
         (name (org-element-property :raw-value el))
         (id (org-entry-get nil "TICKTICK_PROJECT_ID"))
         (color (org-entry-get nil "TICKTICK_PROJECT_COLOR"))
         (view-mode (org-entry-get nil "TICKTICK_PROJECT_VIEWMODE"))
         (kind (org-entry-get nil "TICKTICK_PROJECT_KIND")))
    (ticktick-project-create
     :id id
     :name name
     :color (or color "#F18181")
     :view-mode (or view-mode "list")
     :kind (or kind "TASK"))))

(defun ticktick-common-project-to-org (project)
  "Convert internal PROJECT structure to org heading string.
Returns a formatted org heading with properties."
  (let ((id (ticktick-project-id project))
        (name (ticktick-project-name project))
        (color (ticktick-project-color project))
        (view-mode (ticktick-project-view-mode project))
        (kind (ticktick-project-kind project)))
    (format "* %s\n:PROPERTIES:\n:TICKTICK_PROJECT_ID: %s\n:TICKTICK_PROJECT_COLOR: %s\n:TICKTICK_PROJECT_VIEWMODE: %s\n:TICKTICK_PROJECT_KIND: %s\n:END:\n"
            name
            (or id "")
            (or color "#F18181")
            (or view-mode "list")
            (or kind "TASK"))))

;;; Sync Hash Functions ------------------------------------------------------

(defun ticktick-common-subtree-body-for-hash ()
  "Return a stable string of the current subtree for hash computation.
Removes volatile properties that change on every sync."
  (let* ((raw (buffer-substring-no-properties
               (org-entry-beginning-position) (org-entry-end-position))))
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      ;; Remove property drawers entirely
      (while (re-search-forward "^:PROPERTIES:\\n\\(?:.*\\n\\)*?:END:\\n?" nil t)
        (replace-match "" nil nil))
      ;; Remove any stray volatile property lines
      (goto-char (point-min))
      (while (re-search-forward
              "^:\\(LAST_SYNCED\\|SYNC_CACHE\\|TICKTICK_ETAG\\|TICKTICK_ID\\):.*\\n" nil t)
        (replace-match "" nil nil))
      (buffer-string))))

(defun ticktick-common-should-sync-p ()
  "Return non-nil if the current subtree changed since last sync."
  (let* ((etag   (org-entry-get nil "TICKTICK_ETAG"))
         (cached (org-entry-get nil "SYNC_CACHE"))
         (digest (secure-hash 'sha1 (ticktick-common-subtree-body-for-hash))))
    (or (not etag) (not (and cached (string= cached digest))))))

(defun ticktick-common-update-sync-meta ()
  "Set sync hash and timestamp on current subtree."
  (let* ((digest (secure-hash 'sha1 (ticktick-common-subtree-body-for-hash))))
    (org-set-property "LAST_SYNCED" (format-time-string "%FT%T%z"))
    (org-set-property "SYNC_CACHE"  digest)))

;;; Helper Functions ---------------------------------------------------------

(defun ticktick-common-format-time-for-api (time-string)
  "Format TIME-STRING to TickTick API format (ISO 8601 UTC).
If TIME-STRING is nil, returns nil."
  (when time-string
    (condition-case nil
        (format-time-string "%Y-%m-%dT%H:%M:%S+0000"
                            (date-to-time time-string))
      (error nil))))

(defun ticktick-common-parse-time-from-api (time-string)
  "Parse TIME-STRING from TickTick API format to Emacs time.
If TIME-STRING is nil, returns nil."
  (when (and time-string (not (string-empty-p time-string)))
    (condition-case nil
(date-to-time time-string)
      (error nil))))

(provide 'ticktick-common)
;;; ticktick-common.el ends here
