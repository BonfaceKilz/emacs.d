;; Enable logging when tasks are complete
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE(d!)" "CANCELLED(c@)" "DELAGATED"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; Org-agenda

(global-set-key (kbd "C-c l") 'org-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t)
(setq org-agenda-files (list "~/self/org"))
(setq org-default-notes-file "~/self/org/notes.md")

;; Org habit
(require 'org)
(require 'org-install)
;; (require 'org-habit)
;; (add-to-list 'org-modules "org-habit")
;; (setq org-habit-preceding-days 7
;;       org-habit-following-days 1
;;       org-habit-graph-column 80
;;       org-habit-show-habits-only-for-today t
;;       org-habit-show-all-today t)

(require 'ob)
(require 'ox-hugo-auto-export)

(with-eval-after-load 'ox
  (require 'ox-hugo))

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

(provide 'ob-clojure)

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
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/self/org/journal.org")
         "* Entry-TODO: %?\n" :empty-lines 1)
        ("T" "Todo with Clipboard" entry (file "~/self/org/journal.org")
         "* Entry-TODO: %?\n%U\n   %c" :empty-lines 1)
        ("n" "Note" entry (file "~/self/org/notes.org")
         "* NOTE %?\n%U" :empty-lines 1)
        ("b" "Blog Entry" entry (file "~/self/org/blog.org")
         "* Blog Entry: %?\n%U" :empty-lines 1)
        ("N" "Note with Clipboard" entry (file "~/self/orotesro.org")
         "* NOTE %?\n%U\n   %c" :empty-lines 1)
        ("e" "Event" entry (file+headline "~/self/org/events.org" "Transient")
         "* EVENT %?\n%U" :empty-lines 1)
        ("E" "Event With Clipboard" entry (file+headline "~/self/org/events.org" "Transient")
         "* EVENT %?\n%U\n   %c" :empty-lines 1)
        ("j" "Journal" entry (file+olp+datetree "~/self/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(use-package magit-org-todos
  :config
  (magit-org-todos-autoinsert))

(provide 'setup-org)
