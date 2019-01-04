;; Enable logging when tasks are complete

(setq org-modules '(org-gnus
                    org-id
                    org-habit
                    org-irc
                    org-protocol
                    org-eww
                    org-bbdb
                    org-choose
                    org-panel
                    org-depend))

(require 'org-element)
(require 'org)
(require 'org-compat)
(require 'org-checklist)
(require 'ob)
(require 'ox-hugo-auto-export)
(require 'org-bullets)
(require 'org-indent)

(with-eval-after-load 'ox
  (require 'ox-hugo))

(setq org-log-done t
      org-todo-keywords '((sequence "NEXT(n)" "PLANNING(P)" "INPROGRESS(i)" "WAITING(w)" "|" "ICEBOX(x)" "DONE(d)")
                          (sequence "PHONE(p)" "MEETING(m)" "|" "CANCELLED(c)")
                          (sequence "IDLE(a)"))
      org-todo-keyword-faces
      '(("NEXT" :foreground "blue" :weight bold)
        ("INPROGRESS" :foreground "burlywood" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("ICEBOX" :foreground "orange" :weight normal)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("MEETING" :foreground "yellow" :weight bold)
        ("PHONE" :foreground "yellow" :weight bold)
        ("IDLE" :foreground "magenta" :weight bold)))

(setq org-tag-alist '((:startgroup)
                      ("@STORE" . ?s)
                      ("@WORK" . ?w)
                      ("@HOME" . ?H)
                      (:endgroup)
                      ("WAITING" . ?w)
                      ("HOLD" . ?h)
                      ("PROJECT" . ?P)
                      ("WORK" . ?W)
                      ("HEALTH" . ?F)
                      ("SHOPPING" . ?S)
                      ("NOTE" . ?n)
                      ("CANCELLED" . ?c)
                      ("FLAGGED" . ??)))

(setq org-fast-tag-selection-single-key 'expert)

(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-clock-in-switch-to-state 'clock-in-to-inprogress)
(defun clock-in-to-inprogress (kw)
  "Switch a task from NEXT to INPROGRESS when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-task-p))
      "INPROGRESS")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "INPROGRESS"))))

(defun find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(setq org-startup-indented t)
(diminish 'org-indent-mode)
(setq org-hide-leading-stars nil)
(setq org-link-translation-function nil)
(setq org-cycle-separator-lines 2)

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(setq org-enforce-todo-dependencies t)

(setq org-blank-before-new-entry '((heading)
                                   (plain-list-item . auto)))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings '((default)))

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-deadline-warning-days 30)

(setq org-log-done 'time)

(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

(setq org-habit-graph-column 50)

;; Set up global keys
(global-set-key (kbd "C-c l") 'org-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)

;; Configuring agenda
(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t)
(setq org-agenda-tags-todo-honor-ignore-options t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   (haskell . t)
   (ruby . t)
   (js . t)
   (C . t)))

(add-to-list 'org-src-lang-modes (quote ("dot". graphviz-dot)))
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure
  '((:results . "silent") (:tangle . "yes")))

(defun org-babel-execute:clojure (body params)
  (lisp-eval-string body)
  "Done!")

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook (lambda ()
                                          (condition-case nil
                                              (org-display-inline-images)
                                            (error nil)))
          'append)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

;; org templates
(setq org-default-tasks-file "~/self/org/journal.org")
(setq org-default-notes-file "~/self/org/notes.org")
(setq org-default-lists-file "~/self/org/lists.org")
(setq org-capture-templates
      '(("t" "task" entry (file org-default-tasks-file)
         "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("i" "interrupt" entry (file org-default-tasks-file)
         "* %?\n%U\n%a\n" :clock-in t :clock-resume t :clock-keep t)
        ("P" "Phone call" entry (file org-default-tasks-file)
         "* PHONE Phone call with %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("m" "Meeting" entry (file org-default-tasks-file)
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ("s" "Scheduled Action" entry (file+datetree "~/org/diary.org")
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("R" "Reading Link" entry (file org-default-lists-file)
         "* DONE Read %c :IDLE:\n%U\n" :clock-in t :clock-resume f)
        ("r" "Read later" entry (file+headline "~/self/org/lists.org" "Pocket Entries")
         "* NEXT Read %a :IDLE:\n:PROPERTIES:\n:url: %l\n:END:\n%U\n")
        ("e" "respond" entry (file org-default-notes-file)
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry (file org-default-notes-file)
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("b" "Blog Entry" entry (file "~/self/org/blog.org")
         "* Blog Entry: %?\n%U" :empty-lines 1)
        ("h" "Habit" entry (file org-default-notes-file)
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
        ("e" "Event" entry (file+headline "~/self/org/events.org" "Transient")
         "* EVENT %?\n%U" :empty-lines 1)
        ("j" "Journal" entry (file+olp+datetree "~/self/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(use-package magit-org-todos
  :config
  (magit-org-todos-autoinsert))

(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))

(eval-after-load "org-present"
  '(progn
     (add-hook 'hide-line-hook
               (lambda ()
                 (hide-line-mode)))
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(provide 'setup-org)
(provide 'ob-clojure)
