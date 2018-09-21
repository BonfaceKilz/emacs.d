;; Enable loggin when tasks are complete
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

;; Org-agenda


(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t)
(setq org-agenda-files (list "~/Dropbox/org/personal.org"))

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
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

(provide 'setup-org)