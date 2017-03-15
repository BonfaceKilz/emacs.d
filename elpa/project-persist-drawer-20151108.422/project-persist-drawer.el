;;; project-persist-drawer.el --- Use a project drawer with project-persist.
;;
;; Copyright (c) 2015 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/project-persist-drawer.git
;; Version: 0.0.4
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; #project-persist-drawer
;; Use a project drawer with [project-persist](https://github.com/rdallasgray/project-persist).
;; 
;; ## Usage
;; In your Emacs configuration:
;; ```elisp
;; (require 'project-persist-drawer)
;; (require 'ppd-sr-speedbar) ;; or another adaptor
;; (project-persist-drawer-mode t)
;; ```
;; 
;; ## Adaptors
;; At present only one adaptor is available --
;; [ppd-sr-speedbar](https://github.com/rdallasgray/ppd-sr-speedbar),
;; which uses [sr-speedbar](https://github.com/emacsmirror/sr-speedbar)
;; to display the project drawer.
;; 
;; An adaptor must implement the following functions:
;; ```elisp
;; (eval-after-load 'project-persist-drawer
;;   '(progn
;;     (defun project-persist-drawer--get-window ()
;;       "Return the window associated with the project drawer.")
;; 
;;     (defun project-persist-drawer--open (dir)
;;       "Open the project drawer in DIR.")
;; 
;;     (defun project-persist-drawer--close ()
;;       "Close the project drawer.")))
;; ```
;; 
;; The function declarations should be wrapped in an `eval-after-load` block to ensure project-persist-drawer is loaded first.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'project-persist)

;;;###autoload
(define-minor-mode project-persist-drawer-mode
  "Use a project drawer with project-persist."
  :global t
  :group 'project-persist
  (if project-persist-drawer-mode
      (project-persist-drawer-on)
    (project-persist-drawer-off)))

(defun project-persist-drawer--no-adaptor ()
  "Stub adaptor function."
  (message "project-persist-drawer: no adaptor loaded, \
or adaptor does not provide this function"))

;; Adaptor interface

(defun project-persist-drawer--get-window ()
  "Return the window associated with the project drawer."
  (project-persist-drawer--no-adaptor))

(defun project-persist-drawer--open (dir)
  "Open the project drawer in DIR."
  (project-persist-drawer--no-adaptor))

(defun project-persist-drawer--close ()
  "Close the project drawer."
  (project-persist-drawer--no-adaptor))

;;;

(defvar project-persist-current-project-root-dir)

(defun project-persist-drawer--root ()
  "Get the root directory if available."
  (or (and (boundp 'project-persist-current-project-root-dir)
           project-persist-current-project-root-dir)
      default-directory))

(defun project-persist-drawer-open ()
  "Open the project drawer."
  (interactive)
  (project-persist-drawer--open (project-persist-drawer--root)))

(defun project-persist-drawer-close ()
  "Close the project drawer."
  (interactive)
  (project-persist-drawer--close))

(defun project-persist-drawer-toggle ()
  "Toggle the project drawer."
  (interactive)
  (if (project-persist-drawer--get-window)
      (project-persist-drawer-close)
    (project-persist-drawer-open)))

(defun project-persist-drawer-try-close()
  "Close the drawer if it is open."
  (when (project-persist-drawer--get-window)
    (project-persist-drawer-close)))

(defun project-persist-drawer-on ()
  "Turn on opening of the project drawer on project opening."
  (eval-after-load 'project-persist
    '(progn
       (add-hook 'project-persist-before-save-hook 'project-persist-drawer-try-close)
       (add-hook 'project-persist-before-load-hook 'project-persist-drawer-try-close)
       (add-hook 'project-persist-after-load-hook 'project-persist-drawer-open))))

(defun project-persist-drawer-off ()
  "Turn off opening of the project drawer on project opening."
  (eval-after-load 'project-persist
    '(progn
       (remove-hook 'project-persist-before-save-hook 'project-persist-drawer-try-close)
       (remove-hook 'project-persist-before-load-hook 'project-persist-drawer-try-close)
       (remove-hook 'project-persist-after-load-hook 'project-persist-drawer-open))))

(provide 'project-persist-drawer)
;;; project-persist-drawer.el ends here
