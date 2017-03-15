;;; anything-fix-multiline-process.el

;; Copyright (C) 2013 mechairoi

;; Author: mechairoi

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

;;; Code:

(require 'anything)
(provide 'anything-fix-multiline-process)

(defun anything-insert-candidate-separator (&optional insert-function) ;; optional insert function
  "Insert separator of candidates into the anything buffer."
  (unless insert-function (setq insert-function 'insert))
  (funcall insert-function anything-candidate-separator)
  (put-text-property (point-at-bol)
                     (point-at-eol) 'anything-candidate-separator t)
  (funcall insert-function "\n"))

(defun anything-output-filter--process-source (process string source limit)
  (dolist (candidate (anything-transform-candidates
                      (anything-output-filter--collect-candidates
                       (split-string string "\n")
                       (assoc 'incomplete-line source))
                      source t))
    (if (not (assq 'multiline source))
        (anything-insert-match candidate 'insert-before-markers source)
        (let ((start (point)))
          (unless (= (cdr (assoc 'item-count source)) 0)
            (anything-insert-candidate-separator 'insert-before-markers)) ;; use insert-before-markers
          (anything-insert-match candidate 'insert-before-markers source)
          (put-text-property start (point) 'anything-multiline t)))
    (incf (cdr (assoc 'item-count source)))
    (when (>= (assoc-default 'item-count source) limit)
      (anything-kill-async-process process)
      (return))))

(defun anything-c-filtered-candidate-transformer-file-line-1 (candidate)
  (when (string-match "^\\(.+?\\):\\([0-9]+\\)\\(?::\\|\n \\)\\(.*\\)$" candidate) ;; fix regexp
    (let ((filename (match-string 1 candidate))
          (lineno (match-string 2 candidate))
          (content (match-string 3 candidate)))
      (cons (format "%s:%s\n %s"
                    (propertize filename 'face compilation-info-face)
                    (propertize lineno 'face compilation-line-face)
                    content)
            (list (expand-file-name
                   filename
                   (or (anything-interpret-value (anything-attr 'default-directory))
                       (and (anything-candidate-buffer)
                            (buffer-local-value
                             'default-directory (anything-candidate-buffer)))))
                  (string-to-number lineno) content)))))

;;; anything-fix-multiline-process.el ends here
