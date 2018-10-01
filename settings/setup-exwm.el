;; Load exwm
(require 'exwm)
(require 'exwm-config)
(exwm-config-default)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "eDP-1"
					    1 "eDP-1"
					    2 "HDMI-1"
					    3 "HDMI-1")
      )
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP-1 --right-of HDMI-1 --auto")))
(exwm-randr-enable)

(exwm-randr-enable)

(provide 'setup-exwm)
