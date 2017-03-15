;;; org-toodledo.el --- Toodledo integration for Emacs Org mode

;; Copyright (C) 2011-2012 Christopher J. White

;; Author: Christopher J. White <emacs@grierwhite.com>
;; Created: 7 Sep 2011
;; Version: 2.16
;; Keywords: outlines, data
;; Package-Requires: ((request-deferred "0.2.0") (emacs "24") (cl-lib "0.5"))

;; GNU General Public License v2 (GNU GPL v2),
;; inspired by work from Sacha Chua
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;; This package adds the ability to sync org-mode tasks with
;; Toodledo, a powerful web-based todo list manager that welcomes 3rd
;; party integrations.  (See http://www.toodledo.com/)
;;
;; This version of `org-toodledo' utilizes version 2.0 of the Toodledo API.
;;
;; See https://github.com/christopherjwhite/org-toodledo
;;

;;; Installation:
;;
;; 1. Required Emacs package:
;;       * `request'
;;
;; 2. Put this file in your load path, byte compile the file for best
;;    performance, see `byte-compile-file'.
;;
;; 3. Put the following in your .emacs:
;;
;;    (push "<path-to-this-file>" load-path)
;;    (require 'org-toodledo)
;;    (setq org-toodledo-userid "<toodledo-userid>")      << *NOT* your email!
;;    (setq org-toodledo-password "<toodled-password>")
;;    (setq org-toodledo-file "<org file name>")
;;    ;; Useful key bindings for org-mode
;;    (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-unset-key "\C-o")
;;             (local-set-key "\C-od" 'org-toodledo-mark-task-deleted)
;;             (local-set-key "\C-os" 'org-toodledo-sync)）
;;    (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (local-unset-key "\C-o")
;;             (local-set-key "\C-od" 'org-toodledo-agenda-mark-task-deleted)）

;;; Code:

(require 'org)
(require 'xml)
(require 'json)
(require 'request-deferred)
(require 'org-agenda)
(require 'cl-lib)

;;
;; User customizable variables
;;

(defgroup org-toodledo nil
  "Toodledo integration for Emacs Org mode"
  :prefix "org-toodledo-"
  :group 'org
  :group 'outlines
  :group 'hypermedia)

(defcustom org-toodledo-userid ""
  "UserID from Toodledo (not your e-mail address).
See http://www.toodledo.com/info/api_doc.php"
  :group 'org-toodledo
  :type 'string)

(defcustom org-toodledo-password ""
  "Password for Toodledo."
  :group 'org-toodledo
  :type 'string)

(defcustom org-toodledo-file nil
  "Filename sync with Toodledo."
  :group 'org-toodledo
  :type 'file)

(defcustom org-toodledo-sync-on-save "ask"
  "Action on save of a orgfile with toodledo tasks.
no    - nothing
ask   - ask the user to sync
yes   - always sync"
  :group 'org-toodledo
  :type 'string)

(defcustom org-toodledo-sync-import-new-tasks t
  "Non-nil means import new tasks from the server.
otherwise only edits to existing tasks from the server are
processed."
  :group 'org-toodledo
  :type 'boolean)

(defcustom org-toodledo-status-to-org-map
  '(("Active" . "TODO")
    ("None" . "TODO")
    ("Next Action" . "TODO")
    ("Planning" . "TODO")
    ("Delegated" . "DELEGATED")
    ("Waiting" . "WAITING")
    ("Someday" . "SOMEDAY")
    ("Hold" . "SOMEDAY")
    ("Postponed" . "SOMEDAY")
    ("Canceled" . "CANCELED")
    ("Reference" . "REFERENCE"))
  "Map of Toodledo API 'status' names to `org-mode' TODO states."
  :group 'org-toodledo
  :type '(alist :key-type string :value-type string))

(defcustom org-toodledo-sync-new-completed-tasks nil
  "Non-nil means sync completed tasks into the local buffer.
When nil, new tasks downloaded from the server are not added if
they are already marked completed.  Existing tasks in the buffer
are always updated."
  :group 'org-toodledo
  :type 'boolean)

(defcustom org-toodledo-inhibit-https nil
  "Non-nil means inhibit the use of HTTPS even if it's available."
  :group 'org-toodledo
  :type 'boolean)

(defcustom org-toodledo-flatten-all-tasks nil
  "Non-nil always flatten all tasks.
ignoring any parent/child relationship."
  :group 'org-toodledo
  :type 'boolean)

(defcustom org-toodledo-indent-task-note t
  "Non-nil means indent the task note body according to the level of the task."
  :group 'org-toodledo
  :type 'boolean)

(defcustom org-toodledo-folder-support-mode nil
  "The method for handling folders."
  :group 'org-toodledo
  :type '(choice (const :tag "Store folder as property only" nil)
                 (const :tag "Treat folders as headings" heading)))

(defcustom org-tqoodledo-archive-deleted-tasks nil
  "Non-nil means archive deleted tasks once they are synced to the server."
  :group 'org-toodledo
  :type 'boolean)

(defcustom org-toodledo-archive-completed-tasks nil
  "Non-nil means to archive completed tasks once they are synced to the server."
  :group 'org-toodledo
  :type 'boolean)

(defcustom org-toodledo-post-sync-hook nil
  "Hook(s) to call after synchronization is complete.
This will be run whether the synchronization was successful or not.

Each hook is called with a single argument, the result list:
   (list tot imod idel onew omod odel errors)

Where:
   tot - total number of changes processed
   imod idel inew - number of received modifications, deletions, and new tasks
   onew omod odel - number of sent new, modificaitons, and deletions
   errors - number of errors"
  :group 'org-toodledo
  :type 'hook)

(defcustom org-toodledo-preserve-drawers nil
  "Non-nil means preserve drawer properties in entry text."
  :group 'org-toodledo
  :type 'boolean)

;;
;; Internal variables for tracking org-toodledo state
;;
(defvar org-toodledo-token-expiry nil "Expiry time for authentication token.")
(defvar org-toodledo-token nil "Authentication token.")
(defvar org-toodledo-key nil "Authentication key.")
(defvar org-toodledo-pro nil "Non-nil if Toodledo account is a pro account.")
(defvar org-toodledo-pro-cached nil "Non-nil means pro variable is cached.")
(defvar org-toodledo-test-mode nil "Non-nil used for testing.")
(defvar org-toodledo-sync-message-time 2 "Seconds to pause after displaying sync message.")
(defvar org-toodledo-use-https nil "Use HTTPS for all calls.  This requires pro *and* a patched url-http.el.")
(defvar org-toodledo-log-level 1 "Level of logs to save to log buffer, 0 is error, 1 is info, 2 is debug.")
(defvar org-toodledo-msg-level 1 "Level of logs to also send to message, 0 is error, 1 is info, 2 is debug.")
(defvar org-toodledo-debug nil "Generate debug messages.")
(defvar org-toodledo-folders nil "Map of folder names to ids.")
(defvar org-toodledo-goals nil "Map of goal names to ids.")
(defvar org-toodledo-contexts nil "Map of context names to ids.")
(defvar org-toodledo-archive-deleted-tasks nil)
(defvar org-toodledo-sim-mode nil "Set to t to simulate http posts, used for testing.")
(defvar org-toodledo-last-parsed-response nil "Used to store the last parsed xml response when debug enabled.")
(defvar org-toodledo-errors nil "List of errors for the last operation.")


;; Registered application ID and token for Toodledo API 2.0
(defconst org-toodledo-appid "orgtoodledo2" "Toodledo registered appid for API 2.0.")
(defconst org-toodledo-apptoken "api4e4fbf7454eeb" "Toodledo apptoken associated with appid for API 2.0.")

(defconst org-toodledo-version "2.16")

(defmacro org-toodledo-make-lookup-function (name)
  "Create a lookup function and caching functions for NAME.

variable:  org-toodledo-NAMEs
functions: org-toodledo-get-NAMEs
           org-toodledo-NAME-to-id
           org-toodledo-id-to-NAME"
  (let ((cache-var (concat "org-toodledo-" name "s"))
        (get-func (concat "org-toodledo-get-" name "s"))
        (add-method (concat name "s/add"))
        (get-method (concat name "s/get")))
    (list
     'progn
     `(defun ,(intern get-func) (&optional force)
        ,(concat "Store an alist of (title . id) in `" cache-var "'.
Reload if FORCE is non-nil.")
        (if (or force (null ,(intern cache-var)))
            (setq ,(intern cache-var)
                  (mapcar
                   (lambda (node)
                     (cons
                      (cl-caddar (xml-get-children node 'name))
                      (cl-caddar (xml-get-children node 'id))))
                   (xml-get-children
                    (org-toodledo-call-method ,get-method)
                    (quote ,(intern name)))))
          ,(intern cache-var)))

     `(defun ,(intern (concat "org-toodledo-" name "-to-id")) (item)
        "Return numeric ID for CONTEXT, creating if necessary."
        (let ((lookups ,(list (intern get-func))))
          (if (null (assoc-string item lookups t))
              ;; Create it if it does not yet exist
              (let ((result
                     (org-toodledo-call-method
                      ,add-method
                      (list (cons "name" item)))))
                (if (eq (car result) 'error)
                    (org-toodledo-die
                     (format "Failed to add new %s: %s" ,name item))
                  (setq ,(intern cache-var)
                        (cons
                         (cons item
                               (cl-caddar (xml-get-children
                                        (car (xml-get-children
                                              result
                                              (quote ,(intern name))))
                                        'id)))
                              ,(intern cache-var))
                        lookups ,(intern cache-var)))))
          (cdr (assoc-string item lookups t))))
     `(defun ,(intern (concat "org-toodledo-id-to-" name)) (id)
        "Return name for context by ID."
        (let ((lookups ,(list (intern get-func))))
          (if (null (rassoc id lookups))
              nil
            (car (rassoc id lookups))))))))

(org-toodledo-make-lookup-function "context")
(org-toodledo-make-lookup-function "goal")

(defconst org-toodledo-fields
  '(;; Toodledo recongized fields
    "id" "title" "status" "completed" "repeat" "repeatfrom" "context"
    "duedate" "duetime" "startdate" "starttime" "modified" "folder"
    "goal" "priority" "note" "length" "parent" "tag"
    ;; org-toodledo only fields
    "sync" "hash")
  "All fields related to a task.")

(defconst org-toodledo-fields-check-empty-or-zero
  '("folder" "goal" "context" "length")
  "Fields that should be set to nil if either \"0\" or \"\".")

;; Create a convenience function "org-toodled-task-<field>" for each field
;; of a task

(mapc (lambda (field)
        (if (member field org-toodledo-fields-check-empty-or-zero)
            (eval
             `(defun ,(intern (concat "org-toodledo-task-" field)) (task)
                ,(concat "Return the task property '" field
                         "' for TASK, return nil if \"0\" or \"\"")
                (let ((value (cdr (assoc ,field task))))
                  (if (and value (not (equal value "0"))
                           (not (equal value "")))
                      value
                    nil))))
          
          (eval `(defun ,(intern (concat "org-toodledo-task-" field)) (task)
                   ,(concat "Return the task property '" field "' for TASK")
                   (cdr (assoc ,field task))))))
      org-toodledo-fields)

(defconst org-toodledo-fields-dont-ask
  '(;; Fields that toodledo always returns, thus cannot be asked for
    "id" "title" "modified" "completed"
    ;; org-toodledo only fields
    "sync" "hash")
  "Fields that must not be asked for from the server, either because the server
returns them automatically, or because they are internal only fields")

(defconst org-toodledo-fields-dont-send
  '(;; Toodledo automatically sets modified, so don't attempt to push it
    "modified"
    ;; org-toodledo only fields
    "sync" "hash")
  "Fields that shouldn't be sent to the server.")

(defconst org-toodledo-hash-fields
  '( "title" "status" "completed" "repeat" "repeatfrom" "context"
     "duedate" "duetime" "startdate" "starttime" "folder" "goal"
     "priority" "note" "length" "parent" "tag")
  "Fields that are used to compute the hash of a task for detecting
when a task changed.")

(defconst org-toodledo-hash-fields-skip-if-zero
  '( "duetime" "starttime" "length")
  "Fields that are skipped if they are 0 when computing the hash.
This prevents newly supported fields from causing all tasks to
appear to have been modified.")

(defvar org-toodledo-fields-ask
  (remove nil
          (mapcar
           (lambda (f) (if (member f org-toodledo-fields-dont-ask) nil f))
           org-toodledo-fields))
  "Fields that can be asked for (fields minus `org-toodledo-fields-dont-ask').")

(defvar org-toodledo-fields-send
  (remove nil
          (mapcar
           (lambda (f) (if (member f org-toodledo-fields-dont-send) nil f))
           org-toodledo-fields))
  "Fields that should be encoded and sent for new/modified
tasks (fields minus org-toodled-fields-dont-send)" )

(defconst org-toodledo-error-code-map
  '(("1" missing-key "You did not specify a key for authentication")
    ("2" invalid-key "The authentication key that you provided has expired or is invalid")
    ("3" too-many-tasks "Only 50 tasks can be added/edited/deleted at a time")
    ("4" no-tasks "You didn't specify any tasks to add/edit/delete")
    ("5" empty-task-title "The task's title cannot be blank")
    ("6" max-tasks-reached "The maximum number of tasks allowed per account (20000) has been reached")
    ("7" invalid-task-id "Invalid task ID number")
    ("8" invalid-folder-id "Invalid folder ID")
    ("9" invalid-context-id "Invalid context ID")
    ("10" invalid-goal-id "Invalid goal ID")
    ("11" invalid-location-id "Invalid location ID")
    ("12" no-changes "Nothing was changed. You'll get this error if you attempt to edit a task but don't pass any parameters to edit")
    ("13" invalid-parent-id "Invalid parent ID")
    ("100" unknown-error "Unknown Error")
    ("500" server-offline "The Toodledo server is offline for maintenance")
    ("501" ssl-requires-pro "SSL connections require a Pro subscription"))
  "Map of Toodledo API error codes.")

(defconst org-toodledo-api-status-map
  '(("0" . "None")
    ("1" . "Next Action")
    ("2" . "Active")
    ("3" . "Planning")
    ("4" . "Delegated")
    ("5" . "Waiting")
    ("6" . "Hold")
    ("7" . "Postponed")
    ("8" . "Someday")
    ("9" . "Canceled")
    ("10" . "Reference"))
  "Map of Toodledo API 'status' field values to names for easy reference.
The possible values represent the keys for use in
`org-toodledo-status-to-org-map'" )

(defconst org-toodledo-property-names
  '("ToodledoLastSync"
    "ToodledoLastEdit"
    "ToodledoLastDelete"
    "OrgToodledoVersion"
    "ToodledoID"
    "Hash"
    "ToodledoFolder"
    "ToodledoGoal"
    "ToodledoFolderID"
    "ToodledoSyncError")
  "List of org properties added by org-toodledo.")

(defvar org-toodledo-tmp-ref -1
  "Temporary ID used to tag new tasks when synced in bulk to the server.
These ids should only be used for the short period of
time when a new task is")

;; Replacements for obsolete aput/adelete from assoc.el
(defmacro alist-put (alist key val)
  `(let ((cons (assoc ,key ,alist)))
    (if cons
        (setq ,alist (delq (assoc ,key ,alist) ,alist)))
    (push (cons ,key ,val) ,alist)
    ,alist))

(defmacro alist-delete (alist key)
  `(setq ,alist (delq (assoc ,key ,alist) ,alist)))

;; There's a bug in url-http that seems to attempt connection reuse
;; when the connection is not valid.  This seems to only affect https
;; but just disable if set.
;;
;; See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=9592
;;
(when (boundp 'url-http-inhibit-connection-reuse)
  (setq url-http-inhibit-connection-reuse t))

(defun org-toodledo-initialize (&optional default-heading)
  "Setup current item in an org file with Toodledo tasks.  If not "
  (interactive)
  (when (not (eq major-mode 'org-mode))
    (error "Toodledo initialization must be performed on an org-mode file"))

  (save-excursion
    (if (org-toodledo-goto-base-entry t)
        (org-toodledo-info "Org-toodled already initialized")
      (let ((item default-heading)
            result)
        (setq org-toodledo-folders nil)
        (setq org-toodledo-goals nil)
        (setq org-toodledo-contexts nil)
        (unless item
          (condition-case nil
              (progn
                (org-back-to-heading t)
                (setq item (read-from-minibuffer
                            "Default heading for Toodledo tasks: "
                            (elt (org-heading-components) 4))))
            (error
             (setq item (read-from-minibuffer
                         "Default heading for Toodledo tasks: " "TASKS")))))
        (when item
          (goto-char (point-min))
          (unless (re-search-forward (format "^\*+[ \t]* %s"
                                             (regexp-quote item)) nil t)
            (if (y-or-n-p
                 (format "No heading found matching '%s', create? " item))
                (progn
                  (goto-char (point-min))
                  (insert (concat "* " item "\n"))
                  (forward-line -1))
              (error "Aborted")))

          (org-entry-put (point) "ToodledoLastSync" "0")
          (org-entry-put (point) "OrgToodledoVersion" org-toodledo-version)
          (setq result (org-toodledo-sync nil nil t))
          (goto-char (point-min))
          (re-search-forward (format "^\*+[ \t]* %s" (regexp-quote item)))
          (org-overview)
          (org-content))
        result))))

(defun org-toodledo-reset ()
  "Remove any hint of toodledo from the current file."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward
          (concat "^ *:\\("
                  (mapconcat 'identity org-toodledo-property-names "\\|")
                  "\\):") nil t)
    (org-entry-delete nil (match-string 1)))
  (goto-char (point-min)))

(defun org-toodledo-version ()
  "Display the current version of org-toodledo."
  (message "org-toodledo version %s" org-toodledo-version))

(defun org-toodledo-toggle-debug ()
  "Toggle debug messages.  Messages are sent to the buffer *Org-toodledo-log*."
  (interactive)
  (setq org-toodledo-debug (not org-toodledo-debug))
  (setq org-toodledo-log-level
        (if org-toodledo-debug 3 1))
  (if org-toodledo-debug
      (message "Debug enabled - see buffer *Org-toodledo-log*")
    (message "Debug disabled")))

(defun org-toodledo-clear-cached-vars ()
  "Clear all cached variables such as the token, local list of
folders and contexts, etc.  Call this if switching accounts."
  (interactive)
  (setq org-toodledo-token nil)
  (setq org-toodledo-pro nil)
  (setq org-toodledo-pro-cached nil)
  (setq org-toodledo-folders nil)
  (setq org-toodledo-goals nil)
  (setq org-toodledo-contexts nil)
  (setq org-toodledo-use-https nil))

(defun org-toodledo-check-version ()
  (save-excursion
    (if (not (org-toodledo-goto-base-entry))
        (org-toodledo-initialize)
      (let ((version (or (org-entry-get (point) "OrgToodledoVersion") "0")))
        (if (version= version org-toodledo-version)
            (org-toodledo-info
             "org-toodledo buffer at latest version %s" version)
          (org-toodledo-info
           "org-toodledo is using older version %s than current org-toodledo version %s, upgrading"
           version org-toodledo-version)
          (when (version< version "2.3")
            ;; Fixup tags to eliminate the hyphen, which really
            ;; shouldn't be used in tag / property names
            (goto-char (point-min))
            (while (re-search-forward
                    "Toodledo\\(-\\)\\(lastsync\\|ID\\|lastedit_task\\|lastdelete_task\\)"
                    nil t)
              (let ((str2 (match-string 2)))
                (replace-match "" nil nil nil 1)
                (cond
                 ((string= str2 "lastsync")
                  (replace-match "LastSync" nil nil nil 2))

                 ((string= str2 "lastedit_task")
                  (replace-match "LastEdit" nil nil nil 2))

                 ((string= str2 "lastdelete_task")
                  (replace-match "LastDelete" nil nil nil 2)))))

            (goto-char (point-min))
            (while (re-search-forward ":\\(Modified\\|Sync\\):" nil t)
              (org-entry-delete (point) "Modified")
              (org-entry-delete (point) "Sync")))

          (when (version< version "2.10")
            ;; Prefix Folder/Goal tags with Toodledo
            (goto-char (point-min))
            (while (re-search-forward ":\\(Folder\\|Goal\\):" nil t)
              (replace-match "Toodledo\\1" nil nil nil 1)))

          (when (version< version "2.12")
            ;; Cull "Deleted Tasks" into the OrgToodledoPendingDeletes variable
            (goto-char (point-min))
            (let ((regexp (concat "^\\*+[ \t]+\\(" org-todo-regexp "\\)"))
                  (deleted-tasks-str nil))
              (while (re-search-forward regexp nil t)
                ;; 'task' is the current state of the task at point
                ;; and is parsed from the buffer after all tasks above
                ;; this point have been processed.  That means parent
                ;; tasks either have a toodledoid, or were assigned a
                ;; tmp-ref
                (let* ((task (org-toodledo-parse-current-task))
                       (id (org-toodledo-task-id task))
                       (deleted (org-entry-get (point) "Deleted")))
                  (when deleted
                    (setq deleted-tasks-str
                          (if deleted-tasks-str
                              (concat deleted-tasks-str " " id)
                            id))
                    (org-back-to-heading t)
                    (if org-toodledo-archive-deleted-tasks
                        ;; Archive the task
                        (org-archive-subtree)
                      ;; Just delete the task
                      (org-cut-subtree)))))

              (when deleted-tasks-str
                (org-toodledo-goto-base-entry)
                (org-entry-put
                 (point) "OrgToodledoPendingDeletes"  deleted-tasks-str)))

            (let ((m (org-find-exact-headline-in-buffer "Deleted Tasks")))
              (when m
                (goto-char m)
                (org-cut-subtree))))

          ;; Finally, updated the version of the file
          (if (org-toodledo-goto-base-entry)
              (org-entry-put
               (point) "OrgToodledoVersion" org-toodledo-version)))))))

;;
;; Token / key functions
;;

(defun org-toodledo-token-valid ()
  "Return if `org-toodledo-token' is both non-null and not expired."
  (and org-toodledo-token
       org-toodledo-token-expiry
       (time-less-p (current-time) org-toodledo-token-expiry)))

(defun org-toodledo-token ()
  "Retrieve authentication token valid for four hours.
This token is used for all interaction with the server.  If the token
expires, a new token is automatically retrieved."
  (if (or (string= org-toodledo-userid "")
          (string= org-toodledo-password ""))
      (error "Please set 'org-toodledo-userid' and 'org-toodledo-password or input the password"))

  (if (org-toodledo-token-valid)
      ;; Return cached token
      org-toodledo-token

    ;; Else retrieve a new token
    (let ((url (concat (if org-toodledo-inhibit-https "http" "https")
                       "://api.toodledo.com/2/account/token.php?f=xml"))
          (sig (md5 (concat org-toodledo-userid org-toodledo-apptoken))))
      (org-toodledo-debug "org-toodledo-token: '%s'" url)
      (request url
               :params  `((userid . ,org-toodledo-userid)
                          (appid  . ,org-toodledo-appid)
                          (sig    . ,sig))
               ;; Parse XML in response body:
               :parser (lambda () (libxml-parse-xml-region (point-min) (point-max)))
               :success (cl-function
                         (lambda (&key data  &allow-other-keys)
                           (if (equal (car data) 'error)
                               (progn
                                 (setq org-toodledo-token nil
                                       org-toodledo-key nil
                                       org-toodledo-token-expiry nil)
                                 (error "Could not log in to Toodledo: %s" (elt data 2)))
                             (setq org-toodledo-token
                                   (elt data 2))
                             ;; Set the expiry time to 4 hours from now
                             (setq org-toodledo-token-expiry
                                   (seconds-to-time (+ (float-time) (* 60 60 4)))))
                           )))))org-toodledo-token)

(defun org-toodledo-key ()
  "Return authentication key used for each request."
  (if (and (org-toodledo-token-valid)
           org-toodledo-key)
      ;; Return cached key
      org-toodledo-key
    (progn
      (if (string= org-toodledo-password "")
          (setq org-toodledo-password
                (read-passwd "Enter toodledo password:")))

      ;; Recompute token and key
      (setq org-toodledo-key
            (md5 (concat (md5 org-toodledo-password)
                         org-toodledo-apptoken
                         (org-toodledo-token)))))))

(defun org-toodledo-get-account-info ()
  "Return account information from server."
  (setq org-toodledo-use-https nil)
  (let ((info (org-toodledo-convert-xml-result-to-alist
               (org-toodledo-call-method "account/get"))))
    (setq org-toodledo-pro (string= "1" (cdr (assoc "pro" info))))
    (setq org-toodledo-pro-cached t)

    ;; The variable `url-http-inhibit-connection-reuse' was added as
    ;; part of a patch.  If it is bound, the patch was applied and
    ;; also includes a fix for adding CRLF at the end of post-data
    ;;
    ;; See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=8931
    (setq org-toodledo-use-https
          (and org-toodledo-pro
               (boundp 'url-http-inhibit-connection-reuse)
               (not org-toodledo-inhibit-https)))

    (when org-toodledo-use-https
      (org-toodledo-info "All interaction with toodledo.com will be via HTTPS"))
    info))

(defun org-toodledo-pro ()
  (unless org-toodledo-pro-cached
    (org-toodledo-get-account-info))
  org-toodledo-pro)

(defun org-toodledo-get-tasks (&optional params)
  "Retrieve tasks from server using PARAMS.
Return a list of task alists."
  (alist-put params "fields" (mapconcat 'identity org-toodledo-fields-ask ","))

  (mapcar
   'org-toodledo-convert-xml-result-to-alist
   (xml-get-children
    (org-toodledo-call-method "tasks/get" params)
    'task)))

(defun org-toodledo-get-deleted (&optional params)
  "Retrieve deleted tasks using PARAMS.
Return a list of task alists."
  (mapcar
   'org-toodledo-convert-xml-result-to-alist
   (xml-get-children
    (org-toodledo-call-method "tasks/deleted" params)
    'task)))

;;
;; Implementation notes on how to subtask support :
;;
;; Syncing new tasks to the server is more complex, consider all new tasks:
;;    * TASKS
;;    ** TODO Parent
;;    *** TODO Child1
;;    *** TODO Child2
;;
;; The current sync order will attempt to create Parent, Child1, and
;; Child2 all at the same time.  Looking just for "ToodledoID" in a
;; parent heading will not work because Parent is not assigned an ID
;; until syncing with the server.
;;
;; Modified algorithm:
;;
;;   1. Collect new-child-tasks as a separate set of tasks.  These are
;;      not all child tasks, just those whose parent is not yet synced
;;      Since tasks are processed top-down in the buffer, parents are
;;      guaranteed to be processed before children.
;;
;;        new-child-tasks-alist:
;;            <child-tmp-ref> => <child-task-def>
;;
;;      Keep a map of all children waiting on this parent:
;;
;;        new-parent-new-child-alist:
;;            <parent-tmp-ref> => '(list <child-tmp-ref> <child-tmp-ref>...)
;;
;;   2. Collect new-edit-tasks that were recently modified to have a
;;      parent task that is new.
;;
;;   3. Create new-tasks first (which will include parent)
;;
;;   4. Create new-child-tasks second, but need to find the newly assigned
;;      parent ID
;;
;; Change scenarios
;;
;; 1. Local changes
;;    a. Add new parent task, add new child tasks
;;       - all new-tasks, the parent task will be assigned a tmp ID
;;         before children processed
;;       - children are new, but must wait for parent to get assigned
;;         a real ID
;;    b. Add new child tasks to an existing parent task (parent task
;;       *not* modified)
;;       - the parent task has no changes, child tasks are new and
;;         parent looked up by title
;;       - no hash values involved, all new tasks
;;    c. User moved existing tasks beneath a new parent task
;;       - hash value of child will change *after* the new parent task
;;         is assigned an id or tmp-ref
;;    d. Move existing child tasks beneath an existing parent
;;       - hash value of child will change due to the new relation to
;;         the parent
;;
;; 2. Remote changes
;;    a. Receive a new parent task
;;       - parent task is no different than a regular task
;;    b. Receive an edit to a task, it has a new parent
;;       - task hash will change due to parent
;;       - need to find the parent and move it there
;;    c. Receive an edit to a child task, making it a normal task (no parent)
;;       - task hash will change due to parent
;;       - need to move the task back to the normal new task folder

(defun org-toodledo-sync (&optional skip-import skip-export init)
  "Synchronize tasks with the server bidirectionally asynchronously."
  (interactive)
  (unless org-toodledo-file
    (org-toodledo-error "org-toodledo-file is not set yet"))
  (lexical-let*
      ((buf (find-file-noselect org-toodledo-file))
       (regexp (concat "^\\*+[ \t]+\\(" org-todo-regexp "\\)"))
       (columns-pos (if (and (boundp 'org-columns-begin-marker)
                             org-columns-begin-marker)
                        (marker-position org-columns-begin-marker) nil))
       (skip-import skip-import)
       (skip-export skip-export)
       (init init)
       deferred-obj
       server-edit-tasks
       server-delete-tasks
       new-tasks
       (new-tasks-count 0)
       new-child-tasks-alist
       new-parent-new-child-alist
       edit-child-tasks-alist
       new-parent-edit-child-alist
       edit-tasks
       (delete-tasks
        (let* ((idstr (or  (with-current-buffer buf
                             (save-excursion
                               (org-toodledo-goto-base-entry)
                               (org-entry-get (point)
                                              "OrgToodledoPendingDeletes")))
                           ""))
               (ids (split-string idstr "[ ]")))
          (if (> (length idstr) 0)
              ids
            nil)))
       tasks-by-title-alist
       (errors 0)
       (end nil) ;; Restrict to Toodledo Task heading only?  XXXCJ
       completed-tasks)
    
    (org-toodledo-info "Starting org-toodledo-sync")
    (org-toodledo-debug "  called interactively: %S"
                        (called-interactively-p 'interactive))
    (with-current-buffer buf
      (save-excursion
        (when columns-pos
          (org-columns-quit))
        (org-toodledo-check-version)
        (if (and org-toodledo-sync-new-completed-tasks
                 org-toodledo-archive-completed-tasks)
            (org-toodledo-error
             "org-toodledo-sync-new-completed-tasks set to true, \
will not archive completed tasks"))))
    (deferred:$
      (deferred:next
        (lambda ()
          (unless (org-toodledo-token-valid)
            (request-deferred (concat (if org-toodledo-inhibit-https "http" "https")
                                      "://api.toodledo.com/2/account/token.php?f=xml")
                              :params  `((userid . ,org-toodledo-userid)
                                         (appid  . ,org-toodledo-appid)
                                         (sig    . ,(md5 (concat org-toodledo-userid org-toodledo-apptoken))))
                              ;; Parse XML in response body:
                              :parser (lambda () (libxml-parse-xml-region (point-min) (point-max)))))))
      (deferred:nextc it
        (lambda (res)
          (when res
            (if (equal (car (request-response-data res)) 'error)
                (progn
                  (setq org-toodledo-token nil
                        org-toodledo-key nil
                        org-toodledo-token-expiry nil)
                  (error "Could not log in to Toodledo: %s" (elt (request-response-data res) 2)))
              (setq org-toodledo-token
                    (elt (request-response-data res) 2))
              ;; Set the expiry time to 4 hours from now
              (setq org-toodledo-token-expiry
                    (seconds-to-time (+ (float-time) (* 60 60 4))))))))
      (deferred:nextc it
        (lambda ()
          (deferred:parallel
            (lambda () 
              (org-toodledo-call-async-method "account/get"))
            (lambda () 
              (org-toodledo-call-async-method "folders/get")))))
      (deferred:nextc it
        (lambda (data)
          (setq deferred-obj data)
          (unless skip-import
            (with-current-buffer buf
              (save-excursion
                (when (null org-toodledo-folders)
                  (setq org-toodledo-folders
                        (org-toodledo-convert-xml-to-lookup-list
                         (request-response-data (cadr data)) 'folder)))
                (org-toodledo-goto-base-entry)
                (lexical-let ((local-lastedit-task
                               (or (org-entry-get (point) "ToodledoLastEdit") "0"))
                              (server-lastedit-task
                               (cdr (assoc "lastedit_task"
                                           (org-toodledo-convert-xml-result-to-alist
                                            (request-response-data (car data))))))
                              params)
                  (org-toodledo-debug "Checking for edited tasks (local %S, server %S"
                                      local-lastedit-task server-lastedit-task)
                  (when (> (string-to-number server-lastedit-task)
                           (string-to-number local-lastedit-task))
                    (org-toodledo-info "Server has changes, asking for all modafter=%S"
                                       local-lastedit-task)

                    ;; limit to tasks edited since last sync
                    (alist-put params "modafter" local-lastedit-task)

                    ;; if init, grab only uncompleted, otherwises grab all
                    ;; tasks, completed or not
                    (alist-put params "comp"
                               (if (and init
                                        (not org-toodledo-sync-new-completed-tasks))
                                   "0" "-1"))

                    (alist-put params "fields" (mapconcat 'identity org-toodledo-fields-ask ","))
                    
                    (deferred:$
                      (org-toodledo-call-async-method "tasks/get" params)
                      (deferred:nextc it
                        (lambda (xml)
                          (org-toodledo-info "Task get process start")
                          (with-current-buffer buf
                            (save-excursion
                              (setq server-edit-tasks
                                    (mapcar
                                     'org-toodledo-convert-xml-result-to-alist
                                     (xml-get-children (request-response-data xml) 'task)))
                              ;; Process tasks parent tasks first (filter-child = nil)
                              (mapc (lambda (task) (org-toodledo-process-task task nil))

                                    server-edit-tasks)
                              ;; ...then any child tasks (filter-child = t)
                              (mapc (lambda (task) (org-toodledo-process-task task t))
                                    server-edit-tasks)
                              
                              ;; Now, go through server-edit-tasks and look for
                              ;; completed parents, and archive
                              (when org-toodledo-archive-completed-tasks
                                (mapc (lambda (task) (org-toodledo-check-completed-task task))
                                      server-edit-tasks))

                              ;; Check for deleted tasks on the server
                              (org-toodledo-goto-base-entry)
                              (let ((local-lastdelete-task
                                     (or (org-entry-get (point) "ToodledoLastDelete") "0"))
                                    (server-lastdelete-task
                                     (cdr (assoc "lastdelete_task" (org-toodledo-convert-xml-result-to-alist
                                                                    (request-response-data (car deferred-obj))))))
                                    params)
                                (with-current-buffer buf
                                  (save-excursion
                                    (org-toodledo-debug
                                     "Checking for deleted tasks (local %S, server %S"
                                     local-lastdelete-task server-lastdelete-task)
                                    (when (> (string-to-number server-lastdelete-task)
                                             (string-to-number local-lastdelete-task))
                                      (org-toodledo-info "Server has deletes, asking for all after=%S"
                                                         local-lastdelete-task)

                                      ;; limit to tasks deleted since last sync
                                      (alist-put params "after" local-lastdelete-task)
                                      (setq server-delete-tasks (org-toodledo-get-deleted params))
                                      (mapc (lambda (task) (org-toodledo-delete-local-task
                                                            (org-toodledo-task-id task)))
                                            server-delete-tasks)))))))))))))))))
      (deferred:nextc it
        (lambda ()
            (deferred:wait 500)))
      (deferred:nextc it
        (lambda ()
          (with-current-buffer buf
            (save-excursion
              (unless skip-export
                (goto-char (point-min))
                (org-toodledo-debug
                 "Iterating over all tasks in buffer, looking for changes")
                (while (re-search-forward regexp end t)

                  ;; 'task' is the current state of the task at point and is
                  ;; parsed from the buffer after all tasks above this point
                  ;; have been processed.  That means parent tasks either have
                  ;; a toodledoid, or were assigned a tmp-ref
                  (let* ((task (org-toodledo-parse-current-task))

                         ;; This will be null if the task is not yet known to Toodledo
                         (hash (org-entry-get (point) "Hash"))

                         ;; Computed hash based on the current state of the task
                         (computed-hash (org-toodledo-compute-hash nil task))

                         ;; If flagged as deleted, the task was already in
                         ;; Toodledo and should be flushed
                         (deleted (org-entry-get (point) "Deleted"))

                         ;; Find the parent task, if any -- this is not
                         ;; necessarily the task linked by parent-id (but is a
                         ;; toodledo task), this is literally the up-heading
                         ;; parent.  If the parent task is new, it will have
                         ;; been assigned a tmp-ref by the time its put into
                         ;; tasks-by-title-alist
                         ;;
                         ;; This parent-task is the parsed task alist.  It
                         ;; will have either 'id' set if it's an existing task
                         ;; (known by server), or a 'ref' if it is new waiting
                         ;; to be assigned a real id.
                         ;;
                         ;; Note -- subtasks require pro account subscription
                         (parent-task
                          (if (org-toodledo-do-parent)
                              (cdr
                               (assoc (save-excursion
                                        (if (org-toodledo-up-to-base-parent
                                             "ToodledoID")
                                            (elt (org-heading-components) 4)))
                                      tasks-by-title-alist))))
                         (parent-ref (cdr (assoc "ref" parent-task)))
                         (parent-id (cdr (assoc "id" parent-task))))

                    (org-toodledo-debug "Examining task: '%s'"
                                        (org-toodledo-task-title task))
                    (when parent-task
                      (org-toodledo-debug "  parent task: '%s'"
                                          (org-toodledo-task-title parent-task))
                      (org-toodledo-debug "  parent-parent task id: '%s'"
                                          (cdr (assoc "parent" parent-task))))

                    ;; If parent-task has a parent, clear this task's parent,
                    ;; as Toodledo only supports one level of depth
                    (when (and parent-task
                               (not (string= (cdr (assoc "parent" parent-task)) "0")))
                      (org-toodledo-debug "  too much depth, clearing this tasks parent")

                      (setq parent-task nil
                            parent-ref nil
                            parent-id nil))

                    (cond
                     ((and (null (org-toodledo-task-id task))
                           (null (org-toodledo-task-title task)))
                      ;; A "new" task, but the title is empty, just skip
                      (org-toodledo-debug "  skipping empty TODO, no title"))

                     ((null (org-toodledo-task-id task))
                      ;; Collect a "new" task
                      ;;
                      ;; A new task is any task that does not yet have an
                      ;; assigned Toodeldo-ID
                      ;;
                      ;; Assign a temporary id, send it to the server as
                      ;; "ref", it will be echoed back from the server result
                      ;; with a real toodledoid.  This tmp ID is saved in the
                      ;; task as the ToodledoID, but is always negative so as
                      ;; not to conflict with Toodledo assigned IDs.
                      (let ((tmp-ref (number-to-string
                                      (setq org-toodledo-tmp-ref
                                            (1- org-toodledo-tmp-ref))))
                            (new-task (org-toodledo-limit-fields task)))
                        (org-entry-put (point) "ToodledoID" tmp-ref)
                        (alist-put new-task "ref" tmp-ref)
                        (alist-put tasks-by-title-alist
                                   (org-toodledo-task-title task) new-task)

                        (cond
                         ;; No parent, not a child task, just a new task
                         ((null parent-task)
                          (when (org-toodledo-do-parent)
                            (alist-put new-task "parent" 0))
                          (setq new-tasks (append new-tasks (list new-task)))
                          (org-toodledo-debug "...new task, no parent"))

                         ;; New child task, but parent already is synced and has and ID
                         (parent-id
                          (alist-put new-task "parent" parent-id)
                          (setq new-tasks (append new-tasks (list new-task)))
                          (org-toodledo-debug "...new task, child of task id %S"
                                              parent-id))

                         ;; New child task, but parent is also new
                         (parent-ref
                          ;; Save this task in new-child-task-alist for easy
                          ;; lookup later
                          (alist-put new-child-tasks-alist tmp-ref new-task)

                          ;; Track this child as waiting for this parent
                          (alist-put new-parent-new-child-alist
                                     parent-ref
                                     (append
                                      (cdr
                                       (assoc parent-ref
                                              new-parent-new-child-alist))
                                      (list tmp-ref)))

                          (org-toodledo-debug
                           "...new task, child of new parent task ref %S" parent-ref))

                         (t (org-toodledo-die
                             "New task has a parent, but parent task has neither a tmp-ref nor ID")))))

                     ;; Collect an "edit" task
                     ;;
                     ;; Detected by hash change.  This hash will change if any
                     ;; property of the task changed, including parent.  Note
                     ;; that if the parent is a new task, the parent is
                     ;; assigned a tmp-ref that is stored in ToodledoID
                     ;; property of the parent entry.
                     ((not (string= hash computed-hash))
                      (let ((edit-task (org-toodledo-limit-fields task))
                            (id (org-toodledo-task-id task)))
                        (when (and org-toodledo-archive-completed-tasks
                                   (not org-toodledo-sync-new-completed-tasks)
                                   (org-toodledo-task-is-completed task)
                                   (null parent-task))
                          ;; If archiving completed tasks, save off the parent task
                          ;; so we can come back to it later and archive it
                          (setq completed-tasks (append completed-tasks
                                                        (list edit-task)))
                          (org-toodledo-mark-subtree-done))

                        (cond
                         ;; No parent, not a child task, just an edit task
                         ((null parent-task)
                          (when (org-toodledo-do-parent)
                            (alist-put edit-task "parent" "0"))
                          (alist-put tasks-by-title-alist
                                     (org-toodledo-task-title task)
                                     edit-task)
                          (setq edit-tasks (append edit-tasks (list edit-task)))
                          (org-toodledo-debug "...edit task, not a child"))

                         ;; Edit task, but parent already is synced and has an
                         ;; assigned Toodledo ID.
                         (parent-id
                          (alist-put edit-task "parent" parent-id)
                          (alist-put tasks-by-title-alist
                                     (org-toodledo-task-title task)
                                     edit-task)
                          (setq edit-tasks (append edit-tasks (list edit-task)))
                          (org-toodledo-debug "...edit task, child of parent %S"
                                              parent-id))

                         ;; Edit task, but parent is new
                         (parent-ref
                          (alist-put tasks-by-title-alist
                                     (org-toodledo-task-title task)
                                     edit-task)

                          ;; Save this task in edit-child-task-alist for easy
                          ;; lookup later
                          (alist-put edit-child-tasks-alist id edit-task)

                          ;; Track this child as waiting for this parent
                          (alist-put new-parent-edit-child-alist
                                     parent-ref
                                     (append
                                      (cdr
                                       (assoc parent-ref new-parent-edit-child-alist))
                                      (list id)))

                          (org-toodledo-debug "...edit task, child of new parent %S"
                                              parent-ref))
                         (t
                          (alist-put tasks-by-title-alist
                                     (org-toodledo-task-title task) edit-task)
                          (org-toodledo-die
                           "Edit task has a parent, but parent task has neither a tmp-ref nor ID")))))

                     ;; No action on this task, just save in alist for future reference
                     (t
                      (org-toodledo-debug "...no change")
                      (alist-put tasks-by-title-alist
                                 (org-toodledo-task-title task) task))))))))))
      (deferred:nextc it
        (lambda ()
            (deferred:wait 500)))
      (deferred:nextc it
        (deferred:lambda (x)
          (with-current-buffer buf
            (save-excursion
              (when new-tasks
                (setq new-tasks-count (+ new-tasks-count (length new-tasks)))
                (let ((result (org-toodledo-server-add-tasks new-tasks))
                      next-new-tasks)
                  ;; Reset new-tasks, a second round of new-tasks may be created
                  ;; from new child tasks waiting on this parent
                  (cl-loop
                   for new-task in new-tasks
                   for elem in result
                   do (let ((status (car elem))
                            (data (cdr elem)))
                        (with-current-buffer buf
                          (save-excursion
                            (org-toodledo-info "New Task process started")
                            (cond
                             ((eq status 'error)
                              (setq errors (1+ errors))
                              (org-toodledo-goto-todo-entry (cdr (assoc "ref" new-task)))
                              (org-entry-delete (point) "ToodledoID")
                              (org-toodledo-error-addedit-task "add" data new-task))

                             ((eq status 'task)
                              (let ((ref (cdr (assoc "ref" data)))
                                    (id (cdr (assoc "id" data)))
                                    (parent-id (cdr (assoc "parent" data))))
                                (with-current-buffer buf
                                  (save-excursion
                                    (if (not (org-toodledo-goto-todo-entry ref t))
                                        (progn
                                          (setq errors (1+ errors))
                                          (org-toodledo-error
                                           "Failed to find local copy of new task, server ref '%s' id '%s', task: '%s'"
                                           ref id (org-toodledo-task-title new-task)))
                                      (org-entry-put (point) "ToodledoID" id)
                                      (org-entry-delete (point) "ToodledoSyncError")
                                      (org-toodledo-compute-hash t)
                                      (org-toodledo-info
                                       "Successfully synced new task ID %s / ref %s" id ref)

                                      ;; Look in new-parent-new-child-alist to see if
                                      ;; any new child tasks are waiting for this
                                      ;; parent's id
                                      (dolist (child-tmp-ref
                                               (cdr (assoc ref new-parent-new-child-alist)))
                                        (let ((child-task
                                               (cdr
                                                (assoc child-tmp-ref new-child-tasks-alist))))
                                          (alist-put child-task "parent" id)
                                          (setq next-new-tasks
                                                (append next-new-tasks (list child-task)))
                                          (alist-delete new-parent-new-child-alist ref)))

                                      ;; Look in new-parent-new-child-alist to see if
                                      ;; any new child tasks are waiting for this
                                      ;; parent's id
                                      (dolist (child-id
                                               (cdr (assoc ref new-parent-edit-child-alist)))
                                        (let ((child-task
                                               (cdr (assoc child-id edit-child-tasks-alist))))
                                          (alist-put child-task "parent" id)
                                          (setq edit-tasks
                                                (append edit-tasks (list child-task)))
                                          (alist-delete new-parent-edit-child-alist ref)))))))))))))

                  (setq new-tasks next-new-tasks)

                  (when new-parent-new-child-alist
                    (org-toodledo-die
                     (format "Orphaned new child tasks never got a parent ID: %S"
                             new-parent-new-child-alist)))

                  (when new-parent-edit-child-alist
                    (org-toodledo-die
                     (format "Orphaned edit child tasks never got a parent ID: %S"
                             new-parent-edit-child-alist)))))))
          (when new-tasks
            (deferred:nextc (deferred:wait 500) self))))
      (deferred:nextc it
        (lambda ()
          (deferred:wait 500)))
      (deferred:nextc it
        (lambda ()
          (deferred:parallel
            (lambda ()
              (when edit-tasks
                (with-current-buffer buf
                  (save-excursion
                    (org-toodledo-server-edit-tasks edit-tasks)))))
            (lambda () 
              (when delete-tasks
                (with-current-buffer buf
                  (save-excursion
                    (org-toodledo-server-delete-tasks delete-tasks))))))))
      (deferred:nextc it
        (lambda (xml)
          (with-current-buffer buf
            (save-excursion
              (when edit-tasks
                (cl-loop
                 ;;for edit-task in edit-tasks
                 for elem in (car xml)
                 do (let ((status (car elem))
                          (data (cdr elem)))
                      (cond
                       ((eq status 'error)
                        (setq errors (1+ errors))
                        ;;(org-toodledo-error-addedit-task "edit" data edit-task)
                        )
                       ((eq status 'task)
                        (let ((id (cdr (assoc "id" data))))
                          (if (not (org-toodledo-goto-todo-entry id t))
                              (progn
                                (setq errors (1+ errors)))
                            (org-toodledo-compute-hash t)
                            (org-entry-delete (point) "ToodledoSyncError")
                            (org-toodledo-info
                             "Successfully edited task ID %s" id))))))))
              (when delete-tasks
                (let (id fail title errnum errcode elem del-task)
                  (cl-loop
                   for del-task in delete-tasks
                   for elem in (cadr xml)
                   do (progn
                        (if (not (listp elem))
                            (setq id elem)
                          (setq errnum (cdr elem))
                          (setq errcode (org-toodledo-error-num-to-code errnum))
                          (setq fail (car elem))

                          (org-toodledo-error
                           "Server error code %s '%s' while trying to delete task id %s"
                           errnum (org-toodledo-error-num-to-str errnum) del-task)
                          (setq errors (1+ errors))))))
                (org-toodledo-goto-base-entry)
                (org-entry-delete (point) "OrgToodledoPendingDeletes"))

              (when completed-tasks
                (org-toodledo-debug "Completed tasks to archive: %s" completed-tasks)
                (cl-loop
                 for completed-task in completed-tasks
                 do (progn
                      (org-toodledo-debug "Planning to archive completed task: %s"
                                          (org-toodledo-task-title completed-task))
                      (when (org-toodledo-goto-todo-entry
                             (org-toodledo-task-id completed-task))
                        (org-toodledo-debug "Archiving completed task: %s"
                                            (org-toodledo-task-title completed-task))
                        (org-back-to-heading t)
                        (org-archive-subtree)))))

              (unless skip-import
                (org-toodledo-goto-base-entry)

                ;; Refresh account-info, as it lastedit/lastdelete may have
                ;; changed after sending updates to the server
                (setq account-info (org-toodledo-get-account-info))
                (org-entry-put (point) "ToodledoLastSync" (format "%.0f" (float-time)))
                (org-entry-put (point) "ToodledoLastEdit"
                               (cdr (assoc "lastedit_task" account-info)))
                (org-entry-put (point) "ToodledoLastDelete"
                               (cdr (assoc "lastdelete_task" account-info))))
              (org-align-all-tags)
              (when columns-pos
                (goto-char columns-pos)
                (org-columns))


              (let* ((imod (length server-edit-tasks))
                     (idel (length server-delete-tasks))
                     (onew new-tasks-count)
                     (omod (length edit-tasks))
                     (odel (length delete-tasks))
                     (tot (+ imod idel onew omod odel)))

                (when (called-interactively-p 'interactive)
                  (message (format "tot %d errors %d" tot errors))
                  (cond
                   ((= 0 tot)
                    (org-toodledo-info "Sync complete, no changes")
                    (sit-for org-toodledo-sync-message-time))

                   ((= errors 0)
                    (org-toodledo-info
                     (concat (format "Sync complete, %d changes: " tot)
                             (if (> (+ imod idel) 0)
                                 (concat "recv "
                                         (if (> imod 0) (format "%d mod " imod))
                                         (if (> idel 0) (format "%d del " idel))
                                         (if (> (+ onew omod odel) 0) ", ")))
                             (if (> (+ onew omod odel) 0)
                                 (concat "sent "
                                         (if (> onew 0) (format "%d new " onew))
                                         (if (> omod 0) (format "%d mod " omod))
                                         (if (> odel 0) (format "%d del " odel))))))
                    (sit-for org-toodledo-sync-message-time))

                   (t
                    (display-buffer "*Org-toodledo-log*" t)
                    (message "Errors during synchronization.  See '*Org-toodledo-log*' for details.")
                    (sit-for org-toodledo-sync-message-time))))

                (let ((result (list tot imod idel onew omod odel errors)))
                  (run-hook-with-args 'org-toodledo-post-sync-hook result)
                  result)
                (remove-hook 'before-save-hook 'org-toodledo-save-hook)
                (save-buffer)
                (org-toodledo-info "Sync completeted.")
                (add-hook 'before-save-hook 'org-toodledo-save-hook)
                ))))))))

(defun org-toodledo-parse-current-task ()
  "Parse the org task at point and extract all toodledo related fields.
Return an alist of the task fields."
  (save-excursion
    (org-back-to-heading t)
    (when (and (looking-at org-complex-heading-regexp)
               (match-string 2)) ;; the TODO keyword
      (org-toodledo-debug "org-toodledo-parse-current-task: %s"
                          (match-string 0))
      (let* (info
             (status (match-string-no-properties 2))
             (priority (match-string-no-properties 3))
             (title (match-string-no-properties 4))
             (tags-context (org-get-tags))
             (id (org-entry-get (point) "ToodledoID"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled (org-entry-get nil "SCHEDULED"))
             (closed (org-entry-get nil "CLOSED"))
             (context "0")
             tags)

        (when (and (string= status "DONE")
                 (null closed))
              (org-add-planning-info 'closed (org-current-effective-time))
              (setq closed (org-entry-get nil "CLOSED")))

        ;; tags-context is a list of tags from toodledo
        ;; treate '@<label>' as context, otherwise a tag
        (when tags-context
          (dolist (tag tags-context)
            (if (> (length tag) 0)
                (cond
                 ((string-match (org-re "@\\([[:alnum:]_]+\\)") tag)
                  (setq context
                        (org-toodledo-context-to-id (match-string 1 tag))))
                 (t
                  (setq tags (append tags (list tag))))))))

        (setq info
              (list
               (cons "id" id)
               (cons "title" title)
               (cons "length" (or (org-entry-get (point) "Effort") "0"))
               (cons "context" context)
               (cons "tag" (mapconcat 'identity tags ","))
               (cons "completed"
                     (if (equal status "DONE")
                         (format "%.0f" (org-time-string-to-seconds closed))
                       "0"))
               (cons "status" (org-toodledo-map-status status))
               (cons "priority" (org-toodledo-org-to-priority priority))
               (cons "note"
                     (org-toodledo-entry-note))))

        (org-toodledo-debug "parsed-task: %s" info)

        ;; Set task folder
        (alist-put
         info "folder"
         (cond
          ;; Using headings as folders
          ((eq org-toodledo-folder-support-mode 'heading)
           (org-toodledo-get-folder-id))

          ;; Store folder in ToodledoFolder property
          (t
           (if (org-entry-get nil "ToodledoFolder")
               (org-toodledo-folder-to-id
                (org-entry-get nil "ToodledoFolder")) "0"))))

        (alist-put info "goal"
                   (if (org-entry-get nil "ToodledoGoal")
                       (org-toodledo-goal-to-id
                        (org-entry-get nil "ToodledoGoal")) "0"))

        (alist-put info "repeat" "")
        (alist-put info "repeatfrom" "0")

        ;; Process startdate/starttime from scheduled and
        ;; duedate/duetime from deadline.  Both can have a repeat in
        ;; Toodledo, but org only supports one repeat.  The repeat
        ;; from deadline will override the repeat from scheduled
        ;; if both are present
        (mapc
         (lambda (elem)
             (alist-put info (nth 0 elem) "0")
             (alist-put info (nth 1 elem) "0")
             (when (nth 2 elem)
               ;; Passing t as 2nd arg to
               ;; org-toodledo-time-string-to-seconds adjusts for
               ;; timezone, since duedate/duetime/startdate/starttime
               ;; are expected to float according to local time.  This
               ;; is passed to the server as GMT time.
               ;;
               ;;   "<2012-01-31 Tue>" - no time component -> Noon GMT
               ;;   "<2012-01-31 Tue 08:00>" - time component -> 8:00 GMT
               ;;
               ;; org-toodledo-time-string-to-seconds with t passed as
               ;; 2nd param will give the time as GMT
               (alist-put
                info (nth 0 elem)
                (format "%.0f" (org-toodledo-time-string-to-seconds (nth 2 elem) t)))

               ;; Check for a time component, and if so set the
               ;; duetime as well Note that org-parse-time-string
               ;; returns a list with the 2nd and 3rd items
               ;; representing the minutes and hour.  If no-time
               ;; component was set, it returns nil, otherwise a
               ;; number.  Important to distinguish between 0 and nil,
               ;; as the user may have a deadline of "<2012-01-30 Mon
               ;; 00:00>" which will yield 0 and 0 for hour/minutes.
               (when (cadr (org-parse-time-string (nth 2 elem) t))
                 (alist-put
                  info (nth 1 elem)
                  (format "%.0f" (org-toodledo-time-string-to-seconds (nth 2 elem) t))))

               ;; Add on the repeat
                 (when (org-toodledo-org-to-repeat (nth 2 elem))
                   (alist-put info "repeat" (car (org-toodledo-org-to-repeat (nth 2 elem))))
                   (alist-put info "repeatfrom" (cdr (org-toodledo-org-to-repeat (nth 2 elem))))
                   )))
         `(("startdate" "starttime" ,scheduled)
           ("duedate" "duetime" ,deadline)))

        (when (org-toodledo-do-parent)
          (alist-put info "parent" (org-toodledo-get-parent-id)))
        info))))

(defun org-toodledo-diff-tasks (local-task server-task)
  "Show the user two buffers side by side for LOCAL-TASK and SERVER-TASK.
Ask to pick one, the other, or edit.  Return value is the parsed
task."

  (let ((local-buf (get-buffer-create "*Local Task*"))
        (server-buf (get-buffer-create "*Server Task*"))
        task
        key
        (f (lambda (label buf task)
             (switch-to-buffer buf)
             (set-buffer buf)
             (erase-buffer)
             (org-mode)
             (insert "# " label " task\n")
             (org-toodledo-insert-new-task task 'tmp)

             ;; Need to handle parent-id specially, since it's
             ;; actually not saved as a property, it's passively
             ;; detected by heirarchy
             (org-entry-put (point) "Parent-id" (org-toodledo-task-parent task))
             (org-show-subtree))))
    (save-window-excursion
      (delete-other-windows)
      (funcall f "Server" server-buf server-task)

      (split-window-horizontally)
      (funcall f "Local" local-buf local-task)

      (while (not key)
        (setq key (read-char "Local and Server tasks have both been modified\
, use [l]ocal, [s]erver, or [e]dit? "))
        (cond
         ((eq ?l key) (set-buffer local-buf))
         ((eq ?s key) (set-buffer server-buf))
         ((eq ?e key)
          (goto-char (point-min))
          (insert "# Manually edit changes in this or the other buffer\n\
# Press C-c C-c in one buffer to continue\n")
          (other-window 1)
          (goto-char (point-min))
          (insert "# Manually edit changes in this or the other buffer\n\
# Press C-c C-c in one buffer to continue\n")
          (local-set-key "\C-c\C-c" 'exit-recursive-edit)
          (recursive-edit))
         (t (beep) (setq key nil))))
      (goto-char (point-min))
      (re-search-forward (concat "^\\*+[ \t]+\\(" org-todo-regexp "\\)"))
      (setq task (org-toodledo-parse-current-task))
      ;; Recover the parent-id
      (alist-put task "parent" (org-entry-get (point) "Parent-id"))
      )
    (kill-buffer local-buf)
    (kill-buffer server-buf)
    task))

(defun org-toodledo-up-to-base-parent (&optional with-prop)
  (let (parent-pos)
    (while (org-up-heading-safe)
      (if (or (not with-prop)
              (org-entry-get nil with-prop))
          (setq parent-pos (point))))
    (if (not parent-pos)
        nil
      (goto-char parent-pos)
      t)))

(defun org-toodledo-get-parent-id ()
  "Return the ToodledoID of the immediate parent task.
Requires Pro account subscription"
  (save-excursion
    (or (if (and (org-toodledo-do-parent)
                 (org-toodledo-up-to-base-parent "ToodledoID"))
            (org-entry-get nil "ToodledoID"))
        "0")))

(defun org-toodledo-check-completed-task (task)
  (let* ((parent (org-toodledo-task-parent task))
         (is-child (and parent (not (string= parent "0")))))
    (if (and (not is-child)
             (org-toodledo-task-is-completed task)
             (org-toodledo-goto-todo-entry (org-toodledo-task-id task) t))
        (progn
          (org-toodledo-debug "Archiving task: %s"
                              (org-toodledo-task-title task))
          (org-archive-subtree)))))

(defun org-toodledo-process-task (task filter-child)
  "Process TASK definition, comparing with all currently defined tasks.
- if TASK is not yet known (by id), create a new task
- if TASK is known but local copy is not modified, update the local task
- if TASK is known and local copy was modified, insert TASK as a duplicate

If FILTER-CHILD is t, only process tasks that are children (ie, they
have a non-zero parent).  If nil, only process parent tasks."
  (when (>= org-toodledo-log-level 1)
    (org-toodledo-debug "org-toodledo-process-task: task '%s'"
                        (org-toodledo-task-title task))
    (org-toodledo-debug2 "  task definition: %S" task))
  (let* ((parent (org-toodledo-task-parent task))
         (is-child (and parent (not (string= parent "0")))))
    (when (equal filter-child is-child)
      (save-excursion
        (if (org-toodledo-goto-todo-entry (org-toodledo-task-id task) t)

            ;; Found this entry already -- check hash
            (let* ((hash (org-entry-get (point) "Hash"))
                   (computed-hash (org-toodledo-compute-hash))
                   (touched (not (string= hash computed-hash)))
                   (level (elt (org-heading-components) 0)))
              (org-toodledo-debug
               "Found existing task: (hash %S, computed-hash %S, touched %S, level %S)"
               hash computed-hash touched level)
              (cond

               ;; Not touched locally, and server did modify it;
               ;; delete and recreate
               ((not touched)
                (org-toodledo-debug
                 "Task not modified locally, replacing with server version")
                (org-toodledo-insert-new-task task 'edit))

               (touched
                (org-toodledo-debug
                 "Task modified locally and on server, asking user to resolve")
                (let ((local-task (org-toodledo-parse-current-task)))
                  (setq task (org-toodledo-diff-tasks local-task task))
                  (org-toodledo-debug2 "resolved task: %S" task)
                  (org-toodledo-insert-new-task task 'edit)

                  ;; Clear hash, this will force the resolved result
                  ;; to get sync'd back to the server
                  (org-entry-put (point) "Hash" "0")))))

          ;; Not found, add as new
          (org-toodledo-debug "Task not found locally, inserting as new")
          (if (and org-toodledo-sync-import-new-tasks
                   (or org-toodledo-sync-new-completed-tasks
                       (not (org-toodledo-task-is-completed task)))
                   (or (not org-toodledo-test-mode)
                       (string-match "ORGTOODLEDOTEST"
                                     (org-toodledo-task-title task))))

              (org-toodledo-insert-new-task task 'new)
            (org-toodledo-debug2
             "...skipped: (import-new %S, sync-new-completed %S,\
 is completed %S, test-mode %S, test task %S)"
             org-toodledo-sync-import-new-tasks
             org-toodledo-sync-new-completed-tasks
             (org-toodledo-task-is-completed task)
             org-toodledo-test-mode
             (string-match "ORGTOODLEDOTEST" (org-toodledo-task-title task)))))))))

;;
;; Contexts for inserting a task, and where to put it:
;;
;;   1) Brand new task pulled in from server
;;
;;      - if a parent task is known, put it as a child
;;        - level computed as parent+1
;;
;;      - if no parent, add to the end of the base-entry
;;
;;   2) Edit of an existing task
;;
;;      - parent task may be new, put it as a child, but don't move
;;        it if the existing task was already a proper child
;;        - level computed as parent+1
;;
;;      - else, put it in the same place as the existing task
;;        - level should be taken from the old task
;;
;;   3) Duplicate of an existing task
;;
;;      - parent task may be new, put it as a child
;;        - level computed as parent+1
;;
;;      - else, put it in the same place as the existing task
;;        - level should be taken from the old task
;;
;; If a folder is specified, a heading by the name of the folder is used
;; as the base entry.
;;
(defun org-toodledo-insert-new-task (task mode)
  (save-excursion

    ;; mode:
    ;;   tmp - putting the task in a temp buffer, don't bother with level/point
    ;;   new - put it in appropriate place based on parent, or end of base entry
    ;;   edit - put it at point if just modifiying and no good reason to move it

    (let* ((repeat (org-toodledo-repeat-to-org
                    (org-toodledo-task-repeat task)
                    (org-toodledo-task-repeatfrom task)))
           (taskid (org-toodledo-task-id task))
           (priority (org-toodledo-task-priority task))
           (context (org-toodledo-task-context task))
           (note (org-toodledo-task-note task))
           (duedate (string-to-number (org-toodledo-task-duedate task)))
           (duetime (string-to-number (org-toodledo-task-duetime task)))
           (startdate (string-to-number (org-toodledo-task-startdate task)))
           (starttime (string-to-number (org-toodledo-task-starttime task)))
           (parent (org-toodledo-task-parent task))
           (old-parent (if (eq mode 'edit) (org-toodledo-get-parent-id)))
           (folder-id (org-toodledo-task-folder task))
           (old-folder-id  (if (eq mode 'edit) (org-toodledo-get-folder-id)))
           (goal (org-toodledo-task-goal task))
           (length (org-toodledo-task-length task))
           (tags (split-string (or (org-toodledo-task-tag task) "") " *, *" t))
           (level (if (eq mode 'edit) (elt (org-heading-components) 1)))
           pos deadline scheduled
           (compute-hash t))

      (org-toodledo-debug "org-toodledo-insert-new-task: %s"
                          (org-toodledo-task-title task))

      (when (eq mode 'edit)
        (delete-region
         (progn (org-back-to-heading t) (point))
         (progn (goto-char (match-end 0))
                (if (re-search-forward org-complex-heading-regexp nil t)
                    (goto-char (match-beginning 0))
                  (org-end-of-subtree t t)))))

      ;; Move to the proper location for the new task and compute the
      ;; appropriate level
      (cond
       ;; When mode is tmp, ignore parent, as this is a temporary buffer
       ((eq mode 'tmp)
        (setq level 1))

       ;; Put this task as a direct child if parent is present
       ((and parent (not (string= parent "0")))
        ;; ...but only need to do the move if the parent changed!
        (when (not (string= parent old-parent))
          (org-toodledo-goto-todo-entry parent)
          (setq level (1+ (elt (org-heading-components) 0)))
          (org-end-of-subtree t t)))

       ;; Move to the end of the base entry if parent was
       ;; cleared or brand new task (without a parent)
       ((or
         ;; Old parent set, new parent cleared
         (and old-parent (not (string= old-parent "0"))
              (or (null parent) (string= parent "0")))

         ;; Folder changed
         (and (eq org-toodledo-folder-support-mode 'heading)
              (not (string= folder-id old-folder-id)))

         (and old-parent (not (string= old-parent "0"))
              (or (null parent) (string= parent "0")))

         ;; or brand new task
         (eq mode 'new))

        (if (and (eq org-toodledo-folder-support-mode 'heading) folder-id)
            (org-toodledo-goto-folder-entry folder-id)
          (org-toodledo-goto-base-entry))

        (setq level (1+ (elt (org-heading-components) 0)))
        (org-end-of-subtree t t)))

      (insert (make-string (or level 2) ?*) " " )
      (setq pos (point-marker))

      (insert (concat
               (org-toodledo-task-status-to-org task) " "
               (format "[#%c] " (org-toodledo-priority-to-org priority))
               (org-toodledo-task-title task)
               "\n"))

      ;; duedate => "DEADLINE: <2011-08-21 Sun>"
      ;; If a repeat string was found, it is added:
      ;;   "DEADLINE: <2011-08-21 Sun +1m>"
      (cond
       ((> duedate 0)
        (setq deadline
              (concat org-deadline-string " "
                      (org-toodledo-format-date
                       (if (> duetime 0) duetime duedate)
                       (> duetime 0) repeat))))
       (t
        (setq deadline nil)))

      ;; startdate => "SCHEDULED: <2011-08-21 Sun>"
      ;; If a repeat string was found, it is added:
      ;;   "DEADLINE: <2011-08-21 Sun +1m>"
      (cond
       ((> startdate 0)
        (setq scheduled
              (concat (make-string (if deadline 1 (1+ (or level 2))) ? )
                      org-scheduled-string " "
                      (org-toodledo-format-date
                       (if (> starttime 0) starttime startdate)
                       (> starttime 0) repeat))))
       (t
        (setq scheduled nil)))

      (when (or deadline scheduled)
        (insert (make-string (1+ (or level 2)) ? ))
        (if deadline (insert deadline))
        (if (and deadline scheduled) (insert " "))
        (if scheduled (insert scheduled))
        (insert "\n"))

      ;; note => becomes the task textual contents
      (when note
        (with-temp-buffer
          (insert note)
          (when (not (re-search-forward "\n\\'" nil t))
              (insert "\n"))
          (goto-char (point-min))
          (cond

           ;; Indent the note according to the current level of the TODO item
           (org-toodledo-indent-task-note
            (when (looking-at "[^ ]")
              (while (re-search-forward "^\\( *[^\n ]+\\)" nil t)
                (replace-match
                 (concat (make-string (1+ (or level 2)) ? ) "\\1"))
                (forward-line 1)
                (beginning-of-line))))

           ;; If not indenting, at least watch out for '*'s at the
           ;; beginning of a line and replace with ' *' to ensure it
           ;; does not look like an org heading
           (t
            (while (re-search-forward "^\*" nil t)
              (setq compute-hash nil)
              (replace-match " *"))))

          (setq note (buffer-substring (point-min) (point-max))))

        (org-toodledo-debug2 "Note:\n%s" note)
        (insert note))

      ;; Tags
      (goto-char pos)
      (let ((alltags
             (append
              tags
              (if context
                  (list (concat "@" (org-toodledo-id-to-context context)))
                nil))))
        (if alltags (org-set-tags-to alltags)))

      ;; create a properties drawer for all details
      (goto-char pos)
      (when taskid (org-entry-put (point) "ToodledoID" taskid))

      (if (and folder-id (not (eq org-toodledo-folder-support-mode 'heading)))
          (org-entry-put (point) "ToodledoFolder"
                         (org-toodledo-id-to-folder folder-id))
        (org-entry-delete (point) "ToodledoFolder"))

      (when goal
          (org-entry-put (point) "ToodledoGoal"
                         (org-toodledo-id-to-goal goal)))

      (when length
          (org-entry-put (point) "Effort" (org-toodledo-task-length task)))

      (if compute-hash
          (org-toodledo-compute-hash t)
        (org-entry-put (point) "Hash" "0")))))

(defun org-toodledo-delete-local-task (id)
  "Delete the task text for ID from the current buffer.
This does no interaction with the server.  This is primarily used
when notified that a task on the server was deleted.

In most cases org-toodledo-mark-task-deleted is more appropriate."

  (org-toodledo-debug "org-toodledo-delete-local-task: %S" id)
  (if (and id (not (string= id ""))
           (org-toodledo-goto-todo-entry id t))
      (progn
        (org-back-to-heading t)
        (if org-toodledo-archive-deleted-tasks
            ;; Archive the task
            (org-archive-subtree)

          ;; Just delete the task
          (delete-region
           (point)
           (if (and (end-of-line)
                    (re-search-forward org-complex-heading-regexp nil t))
               (match-beginning 0)
             (org-end-of-subtree t t)
             (point)))))))

(defun org-toodledo-mark-task-deleted ()
  "Marks the current task as deleted.
It will be deleted from the server and from the local org file on
the next sync"
  (interactive "")
  (save-excursion
    (let ((deleted (org-entry-get (point) "Deleted"))
          (start-pos (point-marker))
          (columns-pos (if (and (boundp 'org-columns-begin-marker)
                                org-columns-begin-marker)
                           (marker-position org-columns-begin-marker) nil)))
      (unless (and deleted (string= deleted "1"))
        (when columns-pos
          (org-columns-quit))

        (org-back-to-heading t)
        (let* ((task (org-toodledo-parse-current-task))
               (id (org-toodledo-task-id task))
               response)
          (when (> (length id) 0)
            (org-back-to-heading t)
            (save-excursion
              (org-toodledo-goto-base-entry)
              (let ((deleted-tasks (org-entry-get (point)
                                                  "OrgToodledoPendingDeletes")))
                (org-entry-put (point) "OrgToodledoPendingDeletes"
                               (if deleted-tasks
                                   (concat deleted-tasks " " id)
                                 id))))
            (if org-toodledo-archive-deleted-tasks
                ;; Archive the task
                (org-archive-subtree)
              ;; If not archive, just delete it
              (delete-region
               (point)
               (if (and (end-of-line)
                        (re-search-forward org-complex-heading-regexp nil t))
                   (match-beginning 0)
                 (org-end-of-subtree t t)
                 (point))))))
        (when columns-pos
          (goto-char columns-pos)
          (org-columns)))

      (goto-char start-pos))))

;;
;; Field related functions
;;


(defun org-toodledo-limit-fields (task &optional fields)
  (unless fields
    (setq fields org-toodledo-fields-send))
  (let (new-task)
    (mapc (lambda (key)
            (let ((elem (assoc key task)))
              (when elem (setq new-task (append (list elem) new-task)))))
          fields)
    new-task))

(defun org-toodledo-entry-note ()
  "Extract the note for this task."
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at org-complex-heading-regexp)
      (goto-char (match-end 0))
      (let ((text (buffer-substring-no-properties
                   (point)
                   (if (re-search-forward org-complex-heading-regexp nil t)
                       (match-beginning 0)
                     (org-end-of-subtree t t)))))
        (with-temp-buffer
          (insert text)

          ;; Pull out DEADLINE / SCHEDULED / CLOSED fields
          (dolist (str (list (regexp-quote org-deadline-string)
                             (regexp-quote org-scheduled-string)
                             (regexp-quote org-closed-string)))
            (goto-char (point-min))
            (when (re-search-forward
                   (concat "\\<" str " +[<\[][^]>\n]+[]>][ \t]*") nil t)
              (replace-match "XXXX ")))

          ;; Drop any empty lines with only XXXX
          (goto-char (point-min))
          (while (re-search-forward "^ *\\(XXXX \\)+\n" nil t)
            (replace-match ""))

          ;; Drop any remaining XXXX
          (goto-char (point-min))
          (while (re-search-forward "XXXX " nil t)
            (replace-match ""))

          ;; Remove any hint of org-toodledo properties
          (org-toodledo-reset)

          ;; Remove drawers
          (when (not org-toodledo-preserve-drawers)
            (goto-char (point-min))
            (let ((re org-drawer-regexp)
                  name beg beg-content eol content)
              (while (re-search-forward re nil t)
                (setq name (match-string 1))
                (setq beg (match-beginning 0)
                      beg-content (1+ (point-at-eol))
                      eol (point-at-eol))
                (if (not
                     (and (re-search-forward
                           "^\\([ \t]*:END:[ \t]*\n?\\)\\|^\\*+[ \t]" nil t)
                          (match-end 1)))
                    (goto-char eol)
                  (goto-char (match-end 1))
                  (delete-region beg (point))
                  ))))

          ;; Trim leading/trailing empty lines, but preserve whitepace at the
          ;; beginning of the line
          (goto-char (point-min))
          (if (re-search-forward "\\=\\( *\n\\)+" nil t)
              (replace-match ""))

          (goto-char (point-min))
          (if (re-search-forward "\\( *\n\\)+\\'" nil t)
              (replace-match ""))

          (goto-char (point-max))
          (insert "\n")

          ;; Finally, if this was indented and indenting notes, remove
          ;; indentation
          (when org-toodledo-indent-task-note
            (goto-char (point-min))
            (when (re-search-forward "^ +" nil t)
              (let ((str (match-string 0)))
                (goto-char (point-min))
                (while (re-search-forward (format "^%s" str) nil t)
                  (replace-match "")))))

          (let ((s (buffer-substring-no-properties (point-min)
                                                   (point-max))))
            (if (string-match "\\(\\`[ \t]*[\n\r]+\\)+" s)
                (setq s (replace-match "" t t s)))

            (if (string-match "\\([\n\r]+[ \t]*\\)+\\'" s)
                (setq s (replace-match "" t t s)))
            s))))))

;;
;; Status related functions
;;

(defun org-toodledo-map-status (status &optional to-org)
  (cond
   (to-org
    (if (string-match "^[0-9]+" status)
        (setq status (cdr (assoc status org-toodledo-api-status-map))))
    (cdr (assoc status org-toodledo-status-to-org-map)))
   ((string= status "DONE")
    "0")
   (t
    (car (rassoc
          (car (rassoc status org-toodledo-status-to-org-map))
          org-toodledo-api-status-map)))))

(defun org-toodledo-task-is-completed (task)
  (let ((comp (org-toodledo-task-completed task)))
    (not (or (null comp) (equal comp "") (equal comp "0")))))

(defun org-toodledo-task-status-to-org (task)
  (let ((comp (org-toodledo-task-completed task))
        (status (org-toodledo-task-status task)))
    (cond
     ((not (or (null comp) (equal comp "") (equal comp "0"))) "DONE")
     (t (org-toodledo-map-status status t)))))

;;
;; Map priority
;;

(defun org-toodledo-priority-to-org (priority)
  "Convert PRIORITY from Toodledo to an org-mode letter."
  (min (- ?D (string-to-number priority)) org-lowest-priority))

(defun org-toodledo-org-to-priority (priority)
  "Convert PRIORITY from org-mode priority string.
 '[#A]' to Toodledo priority."
  ;; A=3, B=2, C=1, D=0, E-Z=-1 no priorty=org-default-priority
  (let ((p (if (and priority (string-match "\[#[A-Z]\]" priority))
               (elt priority 2)
             org-default-priority)))
    (number-to-string (max (- ?D p) -1))))

;;
;; Repeat parsing and translation (ie. every 1 month)
;;

;; (assert (equal (org-toodledo-repeat-to-org nil) ""))
;; (assert (equal (org-toodledo-repeat-to-org "Every 1 week") "+1w"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 1 month") "+1m"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 1 year") "+1y"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 1 day") "+1d"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 2 weeks") "+2w"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 2 months") "+2m"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 6 months") "+6m"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 3 months") "+3m"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 3 months" 1) ".+3m"))
;; (assert (equal (org-toodledo-repeat-to-org "Every 1 week" 1) ".+1w"))

(defun org-toodledo-repeat-to-org (repeat &optional from)
  "Turn REPEAT string into org-mode style repeat sequence.
The second argument FROM indicates if the repeat is from the
due-date (0) or from the completion date (1).

The format for REPEAT must be of the form \"Every X T\". Where X
is a number and T is a unit of time (day/week/month/year).

Examples: Every 3 days, Every 1 month, Every 2 years, Every 16 weeks.

Note the Toodlde 2.0 API supports 2 additional formats which are
not supported by this code: \"On the X D of each month\", and
\"Every W\".
"
  (if (not from) (setq from 0))
  (when (stringp from) (setq from (string-to-number from)))
  (cond
   ((null repeat)
    "")
   ((string-match "Every \\([0-9]+\\) day" repeat)
    (concat (if (= from 0) "+" ".+") (match-string 1 repeat) "d"))
   ((string-match "Every \\([0-9]+\\) week" repeat)
    (concat (if (= from 0) "+" ".+")  (match-string 1 repeat) "w"))
   ((string-match "Every \\([0-9]+\\) month" repeat)
    (concat (if (= from 0) "+" ".+")  (match-string 1 repeat) "m"))
   ((string-match "Every \\([0-9]+\\) year" repeat)
    (concat (if (= from 0) "+" ".+") (match-string 1 repeat) "y"))
   (t
    (org-toodledo-error "Unsupported repeat string format: %s" repeat)
    "")))

(defun org-toodledo-org-to-repeat (string)
  "Extract org-mode style repeat information from STRING.
Return as a Toodledo style string.  Return nil if STRING has no
repeat information."
  (if (string-match "\\(\\.?\\)\\+\\([0-9]+\\)\\([wmdy]\\)" string)
      (cons
       (format "Every %s %s" (match-string 2 string)
               (let ((interval (match-string 3 string)))
                 (cond ((string= interval "d") "day")
                       ((string= interval "w") "week")
                       ((string= interval "m") "month")
                       ((string= interval "y") "year"))))
       (format "%d" (length (match-string 1 string))))
    nil))

;;
;; Date Handling
;;

;; (assert (equal (org-toodledo-format-date "2003-08-12" nil)
;;                "<2003-08-12 Tue>"))

(defun org-toodledo-format-date (date addtime &optional repeat)
  "Return yyyy-mm-dd day for DATE."
  (concat
   "<"
   (format-time-string
    (if addtime "%Y-%m-%d %a %H:%M" "%Y-%m-%d %a")
    (cond
     ((listp date) date)
     ((numberp date) (seconds-to-time date))
     ((and (stringp date)
           (string-match "^[0-9]+$" date))
      (seconds-to-time (string-to-number date)))
     (t (apply 'encode-time (org-parse-time-string date))))
    t) ;; This says *universal* time

   (if (and repeat (not (string= repeat ""))) (concat " " repeat) "")
   ">"))

(defun org-toodledo-time-string-to-seconds (timestr &optional univ)
  (+ (org-time-string-to-seconds timestr)
     (if univ (car (current-time-zone)) 0)))

;;
;; Finding TODO tasks
;;

(defun org-toodledo-find-todo-entry (id &optional noerror prop pos)
  "Find entry with property PROP equal to ID.
If PROP is not specified, defaults to ToodledoID.  If POS is t,
return position, otherwise a marker."
  (save-excursion
    (org-toodledo-debug2 "org-toodledo-find-todo-entry: %S %S %S %S"
                         id noerror prop pos)
    (goto-char (point-min))
    (unless prop (setq prop "ToodledoID"))
    (if (re-search-forward (concat "^[ \t]*:" prop ":[ \t]*" id) nil t)
        (progn (org-back-to-heading t)
               (if pos (point) (point-marker)))
      (if noerror
          nil
        (org-toodledo-die "Failed to find todo entry with '%s' = '%s'"
                          prop id)))))

(defun org-toodledo-goto-todo-entry (id &optional noerror prop)
  "Find and goto entry with property PROP equal to ID.
If PROP is not specified, defaults to ToodledoID."
  (let ((pos (org-toodledo-find-todo-entry id noerror prop t)))
    (when pos
      (goto-char pos))))

(defun org-toodledo-find-base-entry (&optional noerror pos)
  "Find base entry with 'ToodledoLastSync' property.
If POS is t, return position, otherwise a marker."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^[ \t]*:ToodledoLastSync:" nil noerror)
        (progn
          (org-back-to-heading t)
          (if pos (point) (point-marker)))
      nil)))

(defun org-toodledo-goto-base-entry (&optional noerror)
  "Find and goto base entry with 'ToodledoLastSync' property."
  (let ((pos (org-toodledo-find-base-entry t t)))
    (when pos (goto-char pos))))

;;
;; Hash Function
;;
(defun org-toodledo-compute-hash (&optional update task)
  "Compute an md5 hash of all user modifyable fields of the current task."
  (if (and task update)
      (org-toodledo-die "Cannot update a task that was passed as an argument"))

  (unless task (setq task (org-toodledo-parse-current-task)))
  (let* ((text
          (mapconcat
           (lambda (field)
             (let ((value (cdr (assoc field task))))
               (if (and (string= value "0")
                        (member field org-toodledo-hash-fields-skip-if-zero))
                   "" value)))
           org-toodledo-hash-fields ""))
         (hash (md5 text)))
    (when update
      (org-entry-put (point) "Hash" hash))
    hash))

;;
;; Agenda Mode Hooks
;;

(defun org-toodledo-agenda-mark-task-deleted ()
  "Mark the task as deleted from an org-agenda buffer."
  (interactive)
  (if (fboundp 'org-agenda-check-type)
      (org-agenda-check-type t 'agenda 'tags))
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-toodledo-mark-task-deleted)))))

;;
;; Save Hook
;;
(defun org-toodledo-save-hook ()
  "Save hook called before saving a file.
If this is an org-mode file and this file has been synced with
Toodledo, check for saving.
 See org-toodledo-sync-on-save."
  (unless org-toodledo-file
    (org-toodledo-error "org-toodledo-file is not set yet"))
  (when (and (eq (current-buffer) (find-file-noselect org-toodledo-file))
    (cond
     ((string= org-toodledo-sync-on-save "ask")
      (y-or-n-p "Sync with Toodledo? "))
     ((string= org-toodledo-sync-on-save "yes") t)
     (t nil)))
    (org-toodledo-sync)))

(add-hook 'before-save-hook 'org-toodledo-save-hook)

;;
;; Folder support
;;

(defun org-toodledo-goto-folder-entry (folder-id)
  "Goto to the headline matching the folder name associated with FOLDER-ID.
If no such folder exists, a new top-level heading is created."
  (let* ((folder-name (org-toodledo-id-to-folder folder-id))
         (marker (org-find-exact-headline-in-buffer folder-name)))
    (unless marker
      (org-toodledo-goto-base-entry)
      (org-end-of-subtree t t)
      (insert "* " folder-name "\n")
      (setq marker (org-find-exact-headline-in-buffer folder-name)))
    (goto-char marker)
    (org-entry-put nil "ToodledoFolderID" folder-id)
    (goto-char marker))
  t)

(defun org-toodledo-get-folder-id-recurse ()
  "This does the work of `org-toodledo-get-folder-id` recursively."

  (if (org-up-heading-safe)
      (let (fid)
        (cond
         ;; Found a task, move up to it's parent
         ((org-entry-get nil "ToodledoID") (org-toodledo-get-folder-id))

         ;; Found a folder
         ((setq fid (org-entry-get nil "ToodledoFolderID")) fid)

         ;; At the base entry
         ((org-entry-get nil "ToodledoLastSync") "0")

         ;; Found a non-task, non-folder heading, convert to a folder
         (t
          (org-back-to-heading t)
          (if (looking-at org-complex-heading-regexp)
              (let ((fid (org-toodledo-folder-to-id
                          (match-string-no-properties 4))))
                (org-entry-put nil "ToodledoFolderID" fid)
                fid)
            (org-toodledo-error "Failed to get task title for folder")))))
    "0"))

(defun org-toodledo-get-folder-id ()
  "Recusive function that walks up from point until either a heading is found that has the 'ToodledlFolderID' property.
  If a bare heading is found (not a TODO and not already a
  folder), that heading is converted into a folder."
  (save-excursion
    (org-toodledo-get-folder-id-recurse)))

(defun org-toodledo-switch-folder-support-mode-to-headings ()
  "Iterate over all tasks.
moving tasks with 'ToodledoFolder' properties to headings based
on the folder naming as follows:

* If a task is a sub-task, just drop the folder property

* If a task has a folder property, move the subtree to a heading by the
same name and drop the folder property
"
  (interactive)
  (org-toodledo-get-folders t)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":ToodledoFolder: *\\(.*\\)$" nil t)
      (let ((folder (match-string 1))
            (task (org-toodledo-parse-current-task)))
        (org-entry-delete (point) "ToodledoFolder")
        (when (string= "0" (org-toodledo-task-parent task))
          ;; Found a task (non-child) that has a folder, move it to
          ;; the right heading
          (save-excursion
            (org-toodledo-goto-folder-entry
             (org-toodledo-folder-to-id folder)))
          (save-excursion
            (org-toodledo-refile-current-task-to-heading folder))))))
  (setq org-toodledo-folder-support-mode 'heading)
  (if (and (called-interactively-p 'interactive)
           (y-or-n-p "Change `org-toodledo-folder-support-mode` to 'heading for future sessions?"))
      (customize-save-variable `org-toodledo-folder-support-mode 'heading)))

;;
;; Miscellaneous
;;

(defun org-toodledo-server-add-tasks (tasks)
  "Add TASKS."
  (org-toodledo-server-addedit-tasks "tasks/add" tasks))

(defun org-toodledo-server-edit-tasks (tasks)
  "Edit TASKS."
  (org-toodledo-server-addedit-tasks "tasks/edit" tasks))

(defun org-toodledo-parse-tasks-xml (xmlresult)
  "Parse the XMLRESULT into a list of task alists of fields."
  ;; xmlresult ::= ((tasks nil <elem> <elem> ...))
  ;; elem      ::= (task nil <taskelem> <taskelem> ...) | <string>
  ;; taskelem  ::= (field nil <string>) | <string>
  ;;
  ;; example: ((tasks nil "  " (task nil "  " (id nil "12345") (ref nil "-234"))))
  (delq nil
        (mapcar
         (lambda (m)
           (if (listp m)
               (cond
                ((eq (car m) 'task)
                 (cons 'task
                       (delq nil
                             (mapcar
                              (lambda (cell)
                                (if (listp cell)
                                    (cons (symbol-name (car cell))
                                          (cl-caddr cell))
                                  nil))
                              (cddr m)))))

                ((eq (car m) 'error)
                 (cons 'error  (cl-cdaadr m))))

             nil))
         (cddr xmlresult))))

(defun org-toodledo-server-addedit-tasks (method tasks)
  "Add/edit TASKS, a list of alists of task fields to set.
This returns a list of alists of fields returned from the
server."
  (org-toodledo-mapsublist
   (lambda (partial-tasks)
     (let (params)
       (alist-put params "tasks" (json-encode-array partial-tasks))
       (when (org-toodledo-do-parent) (alist-put params "fields" "parent"))
       (org-toodledo-parse-tasks-xml
        (org-toodledo-call-method method params))))
   tasks 50))

(defun org-toodledo-server-delete-tasks (taskids)
  "Delete TASKIDS, a list of task ids to delete.
Returns a list of results."
  (org-toodledo-mapsublist
   (lambda (partial-taskids)
     (let (params)
       (alist-put params "tasks" (json-encode-array partial-taskids))
       (delq nil
             (mapcar
              (lambda (m)
                (if (listp m)
                    (if (eq (car m) 'error)
                        (cons 'error (cl-cdaadr m))
                      (cl-caddr m))))
              (cddr (org-toodledo-call-method "tasks/delete" params))))))
   taskids 50))

(defun org-toodledo-call-async-method (method-name &optional params)
  "Call METHOD-NAME with PARAMS and return the parsed XML."
  (lexical-let (send-params
                req
                done
                parsed-response
                (url (concat  (if org-toodledo-use-https "https" "http")
                              "://api.toodledo.com/2/" method-name ".php"))) 
    ;; Convert "unix" to 'unix
    (setq send-params (mapcar (lambda (e)
                                (let ((key (intern (car e)))
                                      (value (cdr e)))
                                  (when (listp value)
                                    (setq value (car value)))
                                  (cons key value))) params))
    (alist-put send-params 'unix "1")
    (alist-put send-params 'key (org-toodledo-key))
    (alist-put send-params 'f "xml")
    (request-deferred url
                      :params send-params
                      ;; Parse XML in response body:
                      :parser (lambda () (libxml-parse-xml-region (point-min) (point-max))))))

(defun org-toodledo-call-method (method-name &optional params)
  "Call METHOD-NAME with PARAMS and return the parsed XML."
  (let (send-params req done parsed-response)
    ;; Convert "unix" to 'unix
    (setq send-params (mapcar (lambda (e)
                                (let ((key (intern (car e)))
                                      (value (cdr e)))
                                  (when (listp value)
                                    (setq value (car value)))
                                  (cons key value))) params))
    (alist-put send-params 'unix "1")
    (alist-put send-params 'key (org-toodledo-key))
    (alist-put send-params 'f "xml")

    (while (not done)
      (let* ((url (concat  (if org-toodledo-use-https "https" "http")
                                   "://api.toodledo.com/2/" method-name ".php")))
        
        (when (eq req nil)
          (request url
                   :params send-params
                   ;; Parse XML in response body:
                   :parser (lambda () (libxml-parse-xml-region (point-min) (point-max)))
                   :success (cl-function
                             (lambda (&key data  &allow-other-keys)
                               (setq parsed-response data)))))
        (setq req t)
        (when (>= org-toodledo-log-level 1)
          (org-toodledo-debug "org-toodledo-call-method: '%s'" url)
          (org-toodledo-debug2
           "\n--- params:\n%S\n--- response:\n%S\n--- parsed response:\n%S\n---"
           send-params parsed-response)
          (setq org-toodledo-last-parsed-response parsed-response))

        (if (eq 'error (car parsed-response))
            (let* ((num (cdr (cl-caadr parsed-response)))
                   (code (org-toodledo-error-num-to-code num)))
                  (org-toodledo-die
                   "Call to %s failed, exceeded max num of retries, giving up"
                   method-name)
              (cond
               ((eq code 'invalid-key)
                (org-toodledo-info
                 "Invalid key error from Toodledo.com, retrying")
                (setq org-toodledo-token nil))

               (t
                (org-toodledo-die
                 (format "Call to %s failed: %s, not retrying"
                         method-name (cl-caddr parsed-response))))))
          (if (eq parsed-response nil)
              (sit-for 0.01)
            (setq done t)))))

    (if (and parsed-response done)
        parsed-response
      (org-toodledo-die (format "Call to %s failed: %s" method-name
                                (cl-caddr parsed-response))))))

(defun org-toodledo-convert-xml-to-lookup-list (xml-resp tag)
  "Parse XML response used for folders, goals, and contexts."
  (mapcar
   (lambda (node)
     (cons
      (cl-caddar (xml-get-children node 'name))
      (cl-caddar (xml-get-children node 'id))))
   (xml-get-children xml-resp tag)))

(defun org-toodledo-get-folders (&optional force)
  "Store an alist of (folder . id) in `org-toodledo-folders'.
Reload if FORCE is non-nil."
  (if (or force (null org-toodledo-folders))
      (setq org-toodledo-folders
            (org-toodledo-convert-xml-to-lookup-list
             (org-toodledo-call-method "folders/get") 'folder)))
  org-toodledo-folders)

(defun org-toodledo-folder-to-id (name)
  "Return numeric ID for NAME, creating if necessary."
  (let ((lookups (org-toodledo-get-folders)))
    (if (null (assoc-string name lookups t))
        ;; Create it if it does not yet exist
        (let ((result (org-toodledo-call-method "folders/add"
                                                (list (cons "name" name)))))
          (if (eq (car result) 'error)
              (org-toodledo-die (format "Failed to add new folder: %s" name))
            (setq org-toodledo-folders nil)
            (setq lookups (org-toodledo-get-folders)))))
    (cdr (assoc-string name lookups t))))

(defun org-toodledo-id-to-folder (id)
  "Return NAME for folder by ID."
  (let ((lookups (org-toodledo-get-folders)))
    (if (null (rassoc id lookups))
        nil
      (car (rassoc id lookups)))))


(defun org-toodledo-convert-xml-result-to-alist (info)
  "Convert INFO to an alist."
  (delq nil
        (mapcar
         (lambda (item)
           (if (listp item)
               (let ((key (symbol-name (car item)))
                     (value (elt item 2)))
                 (cons key (if value (decode-coding-string value 'utf-8))))))
         (xml-node-children (delete "\n\t" info)))))

(defun org-toodledo-refile-current-task (marker)
  (let ((org-refile-targets '((nil . (:level . 3)))))
    (org-refile nil nil (list "Deleted Tasks" (buffer-file-name) nil marker)))
  ;; necessary if the user happened to kill two tasks without moving
  (kill-new ""))

(defun org-toodledo-refile-current-task-to-heading
  (heading &optional parent-heading)

  (let ((marker (org-find-exact-headline-in-buffer heading)) level)
    (when (and (not marker) parent-heading)
      (save-excursion
        (cond
         ((eq t parent-heading)
          (org-toodledo-goto-base-entry))
         (t
          (org-find-exact-headline-in-buffer parent-heading)))

        (setq level (1+ (elt (org-heading-components) 0)))
        (org-end-of-subtree t t)
        (insert (make-string level ?*) " " heading "\n")
        (setq marker (org-find-exact-headline-in-buffer heading))))
    (if marker
        (org-toodledo-refile-current-task marker)
      (org-toodledo-die (format "No such heading %s" heading)))))

(defun org-toodledo-refile-current-task-to-id (id)
  (let ((marker (org-toodledo-find-todo-entry id)))
    (if marker
        (org-toodledo-refile-current-task marker)
      (org-toodledo-die (format "No such task %s" id)))))

(defun org-toodledo-sublist (list from &optional to)
  "Return a sublist of LIST, from FROM to TO.
If END is omitted, it defaults to the length of the sequence.
Counting starts at 0. Like `subseq' and `substring' but solely for
lists."
  ;; Taken from http://osdir.com/ml/help-gnu-emacs-gnu/2009-11/msg00484.html

  ;; start reference
  (let ((start (nthcdr from list)))
    ;; if extract list at the end this makes it much faster
    (if to
        (butlast start (- (+ from (length start)) to))
      start)))

(defun org-toodledo-mapsublist (function list step)
  (let ((len (length list))
        result)
    (cl-do ((offset 0 (+ step offset))) ((> offset len) nil)
      (let ((pr (funcall function
                         (org-toodledo-sublist list offset
                                               (+ offset step)))))
        (setq result (append result pr))))
    result))

(defun org-toodledo-run-sim-tests()
  "Run only simulated org-toodledo tests."
  (interactive)
  (condition-case nil
      (require 'org-toodledo-test)
    (error (error "The file `org-toodledo-test` not available, download directly from source")))
  (org-toodledo-test 'sim))

(defun org-toodledo-run-tests ()
  "Run org-toodledo-test suite."
  (interactive)
  (condition-case nil
      (require 'org-toodledo-test)
    (error (error "The file `org-toodledo-test` not available, download directly from source")))
  (when (y-or-n-p "Switch to test account and run org-toodledo tests? ")
    (let ((old-userid org-toodledo-userid)
          (old-password org-toodledo-password))
      (setq org-toodledo-userid "td4edb814ec9e76")
      (setq org-toodledo-password "org-4-Toodledo")
      (org-toodledo-clear-cached-vars)
      (condition-case nil
          (org-toodledo-test))
      (setq org-toodledo-userid old-userid)
      (setq org-toodledo-password old-password)
      (org-toodledo-clear-cached-vars))))

(defun org-toodledo-log (level str &rest args)
  (let (hdr msg)
    (when (<= level org-toodledo-log-level)
      (with-current-buffer (get-buffer-create "*Org-toodledo-log*")
        (save-excursion
          (goto-char (point-max))
          (setq hdr
                (concat "[" (format-time-string "%H:%M:%S") "] ["
                        (aref ["ERROR" "INFO" "DEBUG" "DEBUG2"] level) "] "))
          (setq msg (apply 'format (append (list str) args)))
          (insert (concat hdr msg "\n"))
          (if (<= level org-toodledo-msg-level)
              (message msg)))))
    msg))

(defun org-toodledo-error (str &rest args)
  (apply 'org-toodledo-log (append (list 0 str) args)))

(defun org-toodledo-die (str &rest args)
  (error (apply 'org-toodledo-log (append (list 0 str) args))))

(defun org-toodledo-info (str &rest args)
  (apply 'org-toodledo-log (append (list 1 str) args)))

(defun org-toodledo-debug (str &rest args)
  (apply 'org-toodledo-log (append (list 2 str) args)))

(defun org-toodledo-debug2 (str &rest args)
  (apply 'org-toodledo-log (append (list 3 str) args)))

(defun org-toodledo-error-addedit-task (type num task)
  "Generate an error message for a failed add or edit of a task."
  (let ((id (org-toodledo-task-id task))
        (title (org-toodledo-task-title task))
        (code (org-toodledo-error-num-to-code num)))
    (save-excursion
      (when (org-toodledo-goto-todo-entry id t)
        (org-entry-put (point) "ToodledoSyncError"
                       (format "(%s) %s" num
                               (org-toodledo-error-num-to-str num)))))
    (org-toodledo-error "Failed to %s task on server, error %s '%s', task %s: '%s'"
                        type num (org-toodledo-error-num-to-str num)
                        (org-toodledo-task-id task)
                        (org-toodledo-task-title task))
    (cond
     ((eq code 'invalid-folder-id)
      (org-toodledo-error "  Task folder: %s, known folders: %s"
                          (org-toodledo-task-folder task)
                          (mapconcat
                           (lambda (p)
                             (concat (cdr p) "='" (car p) "'" ))
                           org-toodledo-folders ", ")))

     ((eq code 'invalid-context-id)
      (org-toodledo-error "  Task context: %s, known contexts: %s"
                          (org-toodledo-task-context task)
                          (mapconcat
                           (lambda (p)
                             (concat (cdr p) "='" (car p) "'"))
                           org-toodledo-contexts ", ")))

     ((eq code 'invalid-goal-id)
      (org-toodledo-error "  Task goal: %s, known goals: %s"
                          (org-toodledo-task-goal task)
                          (mapconcat
                           (lambda (p)
                             (concat (cdr p) "='" (car p) "'" ))
                           org-toodledo-goals ", ")))

     ((eq code 'invalid-parent-id)
      (org-toodledo-error "  Task parent: %s"
                          (org-toodledo-task-parent task))))))

(defun org-toodledo-toggle-sim ()
  "Toggle simulation of http post requests for testing and debug.
See `org-toodledo-sim'."
  (interactive)
  (condition-case nil
      (require 'org-toodledo-sim)
    (error (error "The file `org-toodledo-sim` not available, download directly from source")))
  (setq org-toodledo-sim-mode (not org-toodledo-sim-mode))
  (org-toodledo-clear-cached-vars)
  (if (not org-toodledo-sim-mode)
      (message "org-toodledo http posts are REAL")
    (message "org-toodledo http posts are SIMULATED")))

(defun org-toodledo-error-num-to-code (num)
  (let ((match (assoc num org-toodledo-error-code-map)))
    (if match
        (cadr match)
      "Unknown code")))

(defun org-toodledo-error-num-to-str (num)
  (let ((match (assoc num org-toodledo-error-code-map)))
    (if match
        (cl-caddr match)
      "Unknown code")))

(defun org-toodledo-do-parent ()
  (and (org-toodledo-pro)
       (not org-toodledo-flatten-all-tasks)))

(defun org-toodledo-mark-subtree-done ()
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (let ((limit (point)))
      (save-excursion
        (exchange-point-and-mark)
        (while (> (point) limit)
          (org-todo "DONE")
          (outline-previous-visible-heading 1))
        (org-todo "DONE")))))

(provide 'org-toodledo)
;;; org-toodledo.el ends here
