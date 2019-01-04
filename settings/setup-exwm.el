;; Load exwm
(require 'exwm)

;; Set the initial workspace number.
(setq exwm-workspace-number 4)
;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;; 's-r': Reset
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
;; 's-w': Switch workspace
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
;; 's-N': Switch to certain workspace
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
;; 's-&': Launch application
(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))
;; Line-editing shortcuts
(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])))
;; Enable EXWM
(exwm-enable)

;; Uncomment this when using exwm outside lxde
;; (require 'exwm-systemtray)
;; (setq exwm-systemtray-height 16)
;; (exwm-systemtray-enable)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "DP-1"
                                            1 "eDP-1"
                                            2 "HDMI-1"
                                            3 "HDMI-1")
      )

(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP-1 --right-of DP-1 --auto")))

(defun exwm-change-screen-hook ()
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        default-output)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
          (call-process "xrandr" nil nil nil "--output" default-output "--auto")
        (call-process
         "xrandr" nil nil nil
         "--output" (match-string 1) "--primary" "--auto"
         "--output" default-output "--off")
        (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))

(defun exwm-logout ()
  (interactive)
  (bookmark-save)
  (recentf-save-list)
  (save-some-buffers)
  (start-process-shell-command "logout" nil "gnome-session-quit --logout"))

;; Enable this when using exwm outside lxde
;; (global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)

;; comment out this when using exwm outside lxde
(global-set-key (kbd "C-x C-c") 'exwm-logout)
(exwm-randr-enable)

;; Make gimp floating
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (rename-buffer exwm-class-name t))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (rename-buffer exwm-title t))))

;; Rename buffers to match the x11 window class or title:
(defun exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ":"
           (if (<= (length exwm-title) 50) exwm-title
             (concat (substring exwm-title 0 49) "...")))))

;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

(exwm-input-set-key (kbd "s-r") 'exwm-reset)
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)

(defun exwm-workspace-next ()
  (interactive)
  (let ((next-numb (mod (+ 1 exwm-workspace-current-index) exwm-workspace-number)))
    (exwm-workspace-switch next-numb)))
(exwm-input-set-key (kbd "s-s") 'exwm-workspace-next)
(exwm-input-set-key (kbd "s-j") 'exwm-workspace-next)
(push ?\s-j exwm-input-prefix-keys)

(exwm-input-set-key (kbd "s-o") 'other-window)
(exwm-input-set-key (kbd "s-k") 'other-window)
(push ?\s-o exwm-input-prefix-keys)
(push ?\s-k exwm-input-prefix-keys)

(defmacro exwm-switch-to-workspace-key (ws-num)
  `(progn (exwm-input-set-key (kbd (concat "s-" ,(number-to-string ws-num)))
                              (lambda ()
                                (interactive)
                                (exwm-workspace-switch ,ws-num)))
          (let ((key-num (if (eq 0 ,ws-num)
                             10
                           ,ws-num)))
            (exwm-input-set-key (kbd (concat "s-<f" (number-to-string key-num) ">"))
                                (lambda ()
                                  (interactive)
                                  (exwm-workspace-switch ,ws-num))))))
(exwm-switch-to-workspace-key 1)
(exwm-switch-to-workspace-key 0)

(add-to-list 'display-buffer-alist
             `(,(rx bos " *async command*")
               (display-buffer-no-window)))

(defun background-shell-command (command)
  (interactive (list (read-shell-command "$ ")))
  (async-shell-command command (generate-new-buffer " *async command*")))

(exwm-input-set-key (kbd "s-p")
                    #'background-shell-command)

(provide 'setup-exwm)
