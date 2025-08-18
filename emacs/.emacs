;; -*- mode: elisp -*-

;; set the themes directory

(add-to-list 'custom-theme-load-path "/home/rijan/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; set tab bar-mode to always true
(setq tab-bar-mode t)


;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org. This is default in recent
;; Emacs versions, but uncommenting ensures it works on all versions.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(global-set-key "\C-ca" 'org-agenda)

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
;; this changes the location of the auto-saves to save me from the clutter of auto-save files
;; Directory where you want to save your auto-save files
(defvar my-auto-save-directory (expand-file-name "~/emacs_auto_saves/"))

;; Ensure the auto-save directory exists
(unless (file-exists-p my-auto-save-directory)
  (make-directory my-auto-save-directory t))

;; Set up the transformation rules for auto-save files
(setq auto-save-file-name-transforms
      `((".*" ,my-auto-save-directory t)))

;; this bit of code is supposed to delete autosave files on manaul saving

(setq make-backup-files nil)

(setq delete-auto-save-files t)

;; delete done tasks in org buffer

(defun delete-all-done-tasks ()
  "Delete all DONE tasks in the current buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (when (member (org-get-todo-state) '("DONE"))
       (delete-region (point-at-bol) (1+ (point-at-eol)))))
   "/DONE" 'file))

(defun org-alphabetical-sort ()
  "Alphabetically sort all header levels in the current org-buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-sort-entries nil ?a))
   nil 'tree))


;; --- This is for archiving stuff
;; --- Helper Function ---
;; This function checks if a given headline and its entire subtree are "DONE".

(defun my-org-subtree-is-all-done-p (headline)
  "Check if HEADLINE and all its descendant headlines are in a 'DONE' state.
A headline with no TODO keyword is considered not done.
This function respects the user's `org-done-keywords` variable.
Returns t if the entire subtree is done, nil otherwise."
  (let ((all-done t)
        (todo-kw (org-element-property :todo-keyword headline)))
    ;; 1. Check if the headline itself is in a DONE state.
    ;; It must have a keyword, and that keyword must be in `org-done-keywords`.
    (unless (and todo-kw (member todo-kw org-done-keywords))
      (setq all-done nil))

    ;; 2. If the headline is DONE, recursively check its children.
    ;; We can short-circuit if all-done is already nil.
    (when all-done
      (org-element-map (org-element-contents headline) 'headline
        (lambda (child)
          ;; If we find any child that is not fully done,
          ;; we mark the parent subtree as not done and stop checking.
          (unless (my-org-subtree-is-all-done-p child)
            (setq all-done nil)))))
    all-done))


;; --- Main Interactive Function ---
;; This is the function you will call to perform the archival.

(defun my-org-archive-fully-completed-subtrees ()
  "Archive the highest-level headlines that are DONE and have all children also DONE."
  (interactive)
  (let ((locations-to-archive '()))
    ;; --- PASS 1: Find the top-most, fully completed subtrees ---
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        ;; Check if the current headline and all its descendants are DONE.
        (if (my-org-subtree-is-all-done-p headline)
            (progn
              ;; If yes, add its location to our list.
              (push (org-element-property :begin headline) locations-to-archive)
              ;; And return t to prevent `org-element-map` from descending
              ;; into its children, as the whole parent will be archived.
              t)
          ;; If no, return nil to allow the search to continue in its children.
          nil)))

    ;; --- PASS 2: Archive the identified subtrees from bottom to top ---
    (if locations-to-archive
        (progn
          (save-excursion
            ;; `push` creates a list of locations from the end of the buffer
            ;; to the beginning, so we can iterate through it directly to
            ;; ensure safe modification.
            (dolist (location locations-to-archive)
              (goto-char location)
              (org-archive-subtree)))
          (message "Archived %d fully completed subtrees." (length locations-to-archive)))
      (message "No fully completed subtrees found to archive."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Final Recommended Task Metadata Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'org

  ;; --- 1. Tag Configuration (with Correct Exclusivity) ---
  ;; This setup enables the helpful visual tag buffer when you press C-c C-q.
  (setq org-tag-alist
        '(;; The `t` at the end of the group makes it exclusive.
          (:startgroup . "Priority")
          ("HIGH" . ?h) ("MEDIUM" . ?m) ("LOW" . ?l) ("NoD" . ?n)
          (:endgroup . t)

          (:startgroup . "Personnel")
          ("SOLO" . ?s) ("TEAM" . ?t)
          (:endgroup . t)

          ;; This group has no `t`, so it is NOT exclusive.
          (:startgroup . "Type")
          ("Reading" . ?r) ("Writing" . ?w) ("Programming" . ?p)
          (:endgroup)))

  ;; --- The line below enables "expert" mode. It is commented out ---
  ;; --- so you get the standard, one-key-at-a-time behavior you liked. ---
  ;; (setq org-fast-tag-selection-single-key 'expert)


  ;; --- 2. Time Cost using the Standard 'Effort' Property (in minutes) ---
  ;; This ensures compatibility with built-in Org functions.
  (setq org-effort-durations '(("m" . 1)))

  ;; Provides completion hints for the Effort property.
  (setq org-global-properties
        '(("Effort_ALL" . "5 10 15 20 25 30 45 60 90 120 180")))

  ;; Helper function to quickly set the Effort property.
  (defun my/org-set-effort-minutes (&optional minutes)
    "Set the Effort property (in minutes) on the current Org heading."
    (interactive)
    (unless (org-before-first-heading-p)
      (let* ((current (org-entry-get (point) "Effort"))
             (prompt (if current
                         (format "Effort (minutes) [%s]: " current)
                       "Effort (minutes): "))
             (val (or minutes (read-number prompt nil))))
        (org-entry-put (point) "Effort" (number-to-string (max 0 (round val))))
        (message "Set Effort to %s minutes" (org-entry-get (point) "Effort")))))

  ;; A convenient keybinding in Org buffers to set Effort quickly.
  (define-key org-mode-map (kbd "C-c m") #'my/org-set-effort-minutes)


  ;; --- 3. Column View with Summation ---
  ;; This displays the Effort property and sums it up for parent tasks.
  (setq org-columns-default-format "%50ITEM(Task) %10TODO %10Effort{+}")
)