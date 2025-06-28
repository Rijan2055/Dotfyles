;; -*- mode: elisp -*-

;; set the themes directory

(add-to-list 'custom-theme-load-path "/home/rijan/.emacs.d/themes/")
;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; set tab bar-mode to always true
(setq tab-bar-mode t)
;; set theme
(load-theme 'catppuccin t)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf" default))
 '(delete-selection-mode nil)
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
