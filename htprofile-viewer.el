;;; htprofile-viewer.el --- viewer                   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shun

;; Author: shun <shun@shun-PC>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'htprofile-table)
(require 'htprofile-widgets)

(cl-defstruct (htpviewer-viewer (:constructor htpviewer-make-viewer--internal))
  buffer-name variable-list update-func message)

(cl-defun htpviewer-make-viewer (&key buffer-name variable-list update-func message)
  "VARIABLE-LIST is a list of plists such as
(:symbol my-variable :type integer :description \"description of the variable\" :candidates (foo bar)"
  (cl-check-type buffer-name string)
  ;; (cl-assert (htptable-table-p table))
  (cl-check-type variable-list list)
  (cl-check-type update-func (or null function))
  (cl-check-type message (or null string))
  (htpviewer-make-viewer--internal :buffer-name buffer-name
                                   :variable-list variable-list
                                   :update-func update-func
                                   :message message))

(defun htpviewer-get-buffer (viewer)
  (let ((buffer-name (htpviewer-viewer-buffer-name viewer)))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (setq-local truncate-lines t)
          (setq-local buffer-read-only t)
          (current-buffer)))))

(defun htpviewer-get-clean-buffer (viewer)
  (with-current-buffer (htpviewer-get-buffer viewer)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (current-buffer)))

(defun htpviewer-update-viewer (viewer table)
  (with-current-buffer (htpviewer-get-clean-buffer viewer)
    (let ((update-func (htpviewer-viewer-update-func viewer))
          (inhibit-read-only t))
      (insert-button "update"
                     'action (lambda (button) (funcall update-func))
                     'follow-link t)
      (insert "\n")
      (dolist (variable-data (htpviewer-viewer-variable-list viewer))
        (let* ((symbol (plist-get variable-data :symbol))
               (type (plist-get variable-data :type))
               (description (plist-get variable-data :description))
               (candidates (plist-get variable-data :candidates))
               (variable (make-htpwidget-variable :symbol symbol
                                                  :type type
                                                  :after-update-hook update-func
                                                  :candidates candidates)))
          (insert (format "%s: %s "
                          description
                          (htpwidget-get-variable-value-as-string variable)))
          ;; (insert description)
          ;; (insert ": ")
          ;; (htpwidget-insert-variable-value variable)
          ;; (insert " ")
          (htpwidget-insert-evbutton "edit" (list variable))
          (insert "\n"))
        ;; (insert (format "%s\n" (plist-get variable-data :symbol)))
        )
      (insert "\n")
      (let ((message (htpviewer-viewer-message viewer)))
        (when message
          (insert (format "%s\n" message))))
      (htptable-insert-table table))
    ;; (goto-char (point-min))
    ;; (display-buffer (current-buffer))
    ))

(defun htpviewer-show-viewer (viewer)
  (with-current-buffer (htpviewer-get-buffer viewer)
    (goto-char (point-min))
    (display-buffer (current-buffer))))


(provide 'htprofile-viewer)
;;; htprofile-viewer.el ends here
