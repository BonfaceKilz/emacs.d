;; Initial setup

;; Shrinking fringes to 1 pixel
;; (fringe-mode 1)

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
