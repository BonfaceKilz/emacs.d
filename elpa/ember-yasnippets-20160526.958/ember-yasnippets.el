;;; ember-yasnippets.el --- Snippets for Ember.js development

;; Copyright (C) 2015  Ron White

;; Author: Ron White <ronco@costite.com>
;; Version: 0.0.1
;; Package-Requires: ((yasnippet "0.8.0"))
;; Keywords: tools, abbrev, languages

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a package of snippets for ember.js development in both
;; js-mode and coffee-mode.
;;
;; SNIPPETS
;;
;; Data
;;
;; attr          --> Ember Data Attribute
;; belongs       --> Ember Data belongsTo
;; hasmany       --> Ember Data hasMany
;;
;; Functions
;;
;; find          --> Ember Find
;; find-id       --> Ember Find by id
;; logger-debug  --> Ember Logger Debug
;; run           --> Ember Run
;; runonce       --> Ember Run Once
;;
;; Properties
;;
;; actions       --> Ember Actions
;; alias         --> Ember Computed Alias
;; observer      --> Ember Observer
;; on            --> Ember Event
;; prop          --> Ember Computed Property
;; reads         --> Ember Computed reads
;;
;; TEST Helpers
;;
;; cl            --> Ember click
;; path          --> Ember currentPath
;; rn            --> Ember currentRouteName
;; url           --> currentURL
;; fill          --> Ember fillIn
;; find          --> find
;; key           --> Ember keyEvent
;; trig          --> Ember triggerEvent
;; vi            --> Ember visit
;; and           --> Ember andThen

;;; Code:

(defvar ember-yasnippets-root)
(setq ember-yasnippets-root (file-name-directory (or load-file-name
                                                     (buffer-file-name))))

;;;###autoload
(defun ember-yasnippets-initialize()
  (let ((snippets-dir (expand-file-name "snippets" ember-yasnippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snippets-dir t))
    (yas-load-directory snippets-dir)))

;;;###autoload
(eval-after-load "yasnippet"
  '(ember-yasnippets-initialize))


(provide 'ember-yasnippets)
;;; ember-yasnippets.el ends here
