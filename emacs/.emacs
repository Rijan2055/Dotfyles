 ;; -*- mode: elisp -*-

;; Required libraries
(require 'cl-lib)

;; This is required to access various org functions
(require 'org-element)

;; Set emacs to open the last session
(desktop-save-mode 1)

;; set the themes directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; I am removing the warning message from the scratch buffer because it is no longer useful to me
(setq initial-scratch-message nil)

;; Disable the splash screen (to enable it again, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; FIXED: Properly enable tab bar mode
(tab-bar-mode 1)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)

;; Make Org mode work with files ending in .org. This is default in recent
;; Emacs versions, but uncommenting ensures it works on all versions.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Enhanced TODO keywords with more logical flow
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; Add timestamps when tasks are marked DONE or CANCELLED
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Global key bindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;; Custom settings (keeping your existing ones)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf"
     default))
 '(delete-selection-mode nil)
 '(org-agenda-files '("~/work_in_progress/learning_experiments/org_mode/1.org"))
 '(package-selected-packages '(markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Enhanced auto-save configuration with better error handling
(defvar my-auto-save-directory (expand-file-name "~/emacs_auto_saves/"))

;; Ensure the auto-save directory exists with proper error handling
(condition-case nil
    (unless (file-exists-p my-auto-save-directory)
      (make-directory my-auto-save-directory t))
  (error (message "Warning: Could not create auto-save directory %s" my-auto-save-directory)))

;; Set up the transformation rules for auto-save files
(when (file-exists-p my-auto-save-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,my-auto-save-directory t))))

;; Better backup and auto-save configuration
(setq make-backup-files nil
      delete-auto-save-files t
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;; IMPROVED: Archive completed tasks instead of deleting them
(defun archive-all-done-tasks ()
  "Archive all DONE and CANCELLED tasks in the current buffer to archive file."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(DONE\\|CANCELLED\\)" nil t)
        (org-archive-subtree)
        (setq count (1+ count))))
    (if (> count 0)
        (message "Archived %d completed task%s" count (if (= count 1) "" "s"))
      (message "No completed tasks found to archive"))))

;; IMPROVED: More efficient alphabetical sorting
(defun org-alphabetical-sort-all ()
  "Alphabetically sort all header levels in the current org-buffer efficiently."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (condition-case err
        (progn
          ;; Sort top-level entries
          (org-sort-entries nil ?a)
          ;; Sort all subtrees
          (while (outline-next-heading)
            (when (org-at-heading-p)
              (org-sort-entries nil ?a)))
          (message "Buffer sorted alphabetically"))
      (error (message "Error sorting buffer: %s" (error-message-string err))))))

;; IMPROVED: More robust archiving function with better error handling
(defun my-org-subtree-is-all-done-p (headline)
  "Check if HEADLINE and all its descendant headlines are in a 'DONE' state.
A headline with no TODO keyword is considered not done.
This function respects the user's `org-done-keywords` variable.
Returns t if the entire subtree is done, nil otherwise."
  (let ((all-done t)
        (todo-kw (org-element-property :todo-keyword headline)))
    ;; Check if the headline itself is in a DONE state
    (unless (and todo-kw (member todo-kw org-done-keywords))
      (setq all-done nil))
    
    ;; If the headline is DONE, recursively check its children
    (when all-done
      (let ((children (org-element-map (org-element-contents headline) 'headline 'identity)))
        (dolist (child children)
          (unless (my-org-subtree-is-all-done-p child)
            (setq all-done nil)
            (cl-return)))))  ; Early exit when we find incomplete child
    all-done))

(defun my-org-archive-fully-completed-subtrees ()
  "Archive the highest-level headlines that are DONE and have all children also DONE."
  (interactive)
  (condition-case err
      (let ((locations-to-archive '())
            (buffer-modified (buffer-modified-p)))
        ;; Find the top-most, fully completed subtrees
        (save-excursion
          (goto-char (point-min))
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (headline)
              (when (my-org-subtree-is-all-done-p headline)
                (push (org-element-property :begin headline) locations-to-archive)
                t))))  ; Prevent descent into children
        
        ;; Archive the identified subtrees from bottom to top
        (if locations-to-archive
            (progn
              (save-excursion
                (dolist (location (sort locations-to-archive '>))  ; Sort descending for safe deletion
                  (goto-char location)
                  (when (org-at-heading-p)  ; Safety check
                    (org-archive-subtree))))
              (message "Archived %d fully completed subtree%s" 
                      (length locations-to-archive)
                      (if (= (length locations-to-archive) 1) "" "s")))
          (message "No fully completed subtrees found to archive")))
    (error (message "Error archiving subtrees: %s" (error-message-string err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhanced Task Metadata Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unverified, Rijan: The tag shortcuts (single keys like ?h for "HIGH") defined in org-tag-alist (starting at line 176) are used in the interactive tag selection prompt when you run org-set-tags-command, which is bound to C-c C-q by default in Org mode—no custom binding is needed in the config.

(with-eval-after-load 'org
  ;; Enhanced tag configuration with better organization
  (setq org-tag-alist
        '((:startgroup . "Priority")
          ("HIGH" . ?h) ("MEDIUM" . ?m) ("LOW" . ?l) ("SOMEDAY" . ?s)
          (:endgroup . t)
          
          (:startgroup . "Context")
          ("@home" . ?H) ("@work" . ?W) ("@computer" . ?c) ("@phone" . ?p) ("@errands" . ?e)
          (:endgroup)
          
          (:startgroup . "Personnel")
          ("SOLO" . ?1) ("TEAM" . ?2) ("DELEGATED" . ?d)
          (:endgroup . t)
          
          (:startgroup . "Type")
          ("Reading" . ?r) ("Writing" . ?w) ("Programming" . ?P) ("Meeting" . ?M)
          (:endgroup)))
  
  ;; Enhanced effort configuration
  (setq org-effort-durations '(("min" . 1) ("h" . 60) ("d" . 480)))  ; 8-hour workday
  
  (setq org-global-properties
        '(("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 8:00")))
  
  ;; IMPROVED: Better effort setting function with validation
  (defun my/org-set-effort-minutes (&optional minutes)
    "Set the Effort property (in minutes) on the current Org heading with validation."
    (interactive)
    (if (org-before-first-heading-p)
        (message "Not at an org heading")
      (let* ((current-effort (org-entry-get (point) "Effort"))
             (current-minutes (if current-effort 
                                 (org-duration-to-minutes current-effort) 
                                 nil))
             (prompt (if current-minutes
                        (format "Effort (minutes) [current: %d]: " current-minutes)
                      "Effort (minutes): "))
             (val (or minutes 
                     (read-number prompt (or current-minutes 30)))))
        (when (and val (> val 0))
          (let ((effort-string (org-duration-from-minutes (round val))))
            (org-entry-put (point) "Effort" effort-string)
            (message "Set Effort to %s (%d minutes)" effort-string (round val)))))))
  
  ;; Enhanced keybindings in org-mode
  (define-key org-mode-map (kbd "C-c m") #'my/org-set-effort-minutes)
  (define-key org-mode-map (kbd "C-c C-x a") #'archive-all-done-tasks)
  (define-key org-mode-map (kbd "C-c C-x A") #'my-org-archive-fully-completed-subtrees)
  
  ;; Enhanced column view with better formatting
  (setq org-columns-default-format "%50ITEM(Task) %10TODO %10Effort{+} %10CLOCKSUM %16TIMESTAMP_IA")
  
  ;; FIXED: Better agenda configuration without undefined functions
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "STARTED" ((org-agenda-overriding-header "In Progress")))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting For")))
            (tags-todo "HIGH" ((org-agenda-overriding-header "High Priority (includes unscheduled)")
                              (org-agenda-todo-ignore-scheduled nil)
                              (org-agenda-todo-ignore-deadlines nil)))
            (tags-todo "-HIGH+MEDIUM" ((org-agenda-overriding-header "Medium Priority (unscheduled)")
                                      (org-agenda-todo-ignore-scheduled 'all)
                                      (org-agenda-todo-ignore-deadlines 'all)))
            (todo "TODO" ((org-agenda-overriding-header "Other TODOs (unscheduled)")
                         (org-agenda-todo-ignore-scheduled 'all)
                         (org-agenda-todo-ignore-deadlines 'all)
                         (org-agenda-todo-ignore-with-date 'all)
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":HIGH:\\|:MEDIUM:"))))))
          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)
                       (org-agenda-start-on-weekday 1)))
            (stuck "")
            (todo "DONE" ((org-agenda-overriding-header "Completed This Week")))))))
  
  ;; Clock configuration for time tracking
  (setq org-clock-idle-time 15
        org-clock-in-resume t
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-report-include-clocking-task t)
  
  ;; Initialize clock persistence
  (org-clock-persistence-insinuate)
  
  ;; IMPROVED: Better archive configuration with directory creation
  (let ((archive-dir (file-name-directory "~/work_in_progress/learning_experiments/org_mode/archive.org")))
    (unless (file-exists-p archive-dir)
      (make-directory archive-dir t)))
  (setq org-archive-location "~/work_in_progress/learning_experiments/org_mode/archive.org::* Archived Tasks")
  
  ;; Capture templates for quick task entry
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "~/work_in_progress/learning_experiments/org_mode/1.org" "Inbox")
           "* TODO %?\n  SCHEDULED: %t\n  %a\n")
          ("n" "Note" entry (file+headline "~/work_in_progress/learning_experiments/org_mode/1.org" "Notes")
           "* %? :NOTE:\n  %U\n  %a\n")
          ("m" "Meeting" entry (file+headline "~/work_in_progress/learning_experiments/org_mode/1.org" "Meetings")
           "* TODO %? :Meeting:\n  SCHEDULED: %t\n")))
  
  ;; Enable some useful minor modes
  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode 1)
                             (org-indent-mode 1)
                             (auto-fill-mode 0))))  ;; Disable auto-fill in favor of visual-line

;;; --- Part 1: Helper function to map tags to their groups ---

(defun custom-org-get-tag-group-map ()
  "Create a hash table mapping tags to their group from `org-tag-alist`.
The keys are the tags (e.g., \"High\") and the values are their
parent group names (e.g., \"priority\")."
  (let ((tag-map (make-hash-table :test 'equal)))
    ;; Go through each group definition in org-tag-alist
    (dolist (group-entry org-tag-alist)
      ;; A group definition is a cons cell like ("group" . ("tag1" ...))
      (when (and (consp group-entry) (consp (car group-entry)))
        (let* ((group-name (caar group-entry))
               (tags (cdar group-entry)))
          ;; For each tag in the group, add it to our map
          (dolist (tag tags)
            (puthash tag group-name tag-map)))))
    tag-map))


;;; --- Part 2: Core logic to parse headlines and collect data ---

(defun custom-org-collect-task-data ()
  "Collect data from all headlines in the current Org buffer for the custom view.
Returns a list of lists, where each inner list represents a row in the table."
  (let ((tag-group-map (custom-org-get-tag-group-map))
        (table-data '()))
    ;; Use org-map-entries to iterate over all headlines in the buffer.
    ;; The 't' argument means match all headlines.
    (org-map-entries
     (lambda ()
       (let* (;; Parse the headline element at the current position
              (element (org-element-at-point))
              ;; --- Extract data using org-element and other helpers ---
              (task-title (org-element-property :title element))
              (tags (org-element-property :tags element))
              (effort (org-entry-get (point) "Effort"))
              (deadline-struct (org-element-property :deadline element))
              (deadline-str (if deadline-struct
                                (format-time-string "%m/%d/%Y" (org-element-timestamp-to-time deadline-struct))
                              "NA"))
              ;; --- Initialize row data with defaults ---
              (row `(,(or task-title "NO TITLE") ; Task
                     "NA"                         ; Personnel
                     "NA"                         ; Type
                     "NA"                         ; Priority
                     ,(or effort "NA")           ; Time estimate
                     ,deadline-str              ; <deadline>
                     "0.00"                       ; weight (placeholder for now)
                     )))

         ;; --- Populate tag-based columns ---
         (dolist (tag tags)
           (let ((group (gethash tag tag-group-map)))
             (cond
              ((equal group "personnel") (setf (nth 1 row) tag))
              ((equal group "type") (setf (nth 2 row) tag))
              ((equal group "priority") (setf (nth 3 row) tag)))))

         ;; Add the fully processed row to our collected data
         (push row table-data)))
     'all) ; Match all headlines
    ;; Return the data, reversed to be in document order
    (nreverse table-data)))


;;; --- Part 3: User-facing command to generate and display the table ---

(defun custom-org-create-tabular-view ()
  "Generate a custom tabular view of the current Org buffer's tasks."
  (interactive)
  ;; Define the table header
  (let* ((header '("Task" "Personnel" "Type" "Pirority" "Time estimate" "<deadline>" "weight"))
         (data (custom-org-collect-task-data))
         (view-buffer (get-buffer-create "*Custom Org Task View*")))

    ;; Switch to the view buffer to populate it
    (with-current-buffer view-buffer
      (erase-buffer)
      ;; Insert the header
      (insert "| " (mapconcat #'identity header " | ") " |\n")
      ;; Insert the horizontal rule
      (insert "|-" (mapconcat (lambda (x) (make-string (length x) ?-)) header "-|-") "-|\n")
      ;; Insert the data rows
      (dolist (row data)
        (insert "| " (mapconcat #'identity row " | ") " |\n")))

    ;; Display the new buffer to the user
    (switch-to-buffer view-buffer)))
