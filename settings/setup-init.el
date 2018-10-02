;; Initial setup
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Shrinking fringes to 1 pixel
(fringe-mode 7)

;; Remove security vulnerability
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

;; No splash screen
(setq inhibit-splash-screen t
      inhibit-scratch-message nil
      initial-major-mode 'org-mode)

(elpy-enable)
(provide 'setup-init)
