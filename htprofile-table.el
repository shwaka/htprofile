;;; htprofile-table.el --- create table              -*- lexical-binding: t; -*-

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

;;; column format
(cl-defstruct (htptable-col-format (:constructor htptable-make-col-format--internal))
  header width data-formatter truncation)

(cl-defun htptable-make-col-format (&key header width data-formatter truncation)
  (cl-assert (stringp header))
  (cl-assert (integerp width))
  (cl-assert (functionp data-formatter))
  (htptable-make-col-format--internal :header header
                                      :width width
                                      :data-formatter data-formatter
                                      :truncation truncation))

(defface htptable-header-face
  '((t :inverse-video t))
  "face for header")

(defun htptable-format-header (col-format)
  (let* ((width (htptable-col-format-width col-format))
         (orig-header (htptable-col-format-header col-format))
         (truncated-header (truncate-string-to-width (format (format "%%-%ds" width)
                                                             orig-header)
                                                     width)))
    truncated-header
    ;; (propertize truncated-header 'face 'htptable-header-face)
    ))

(defun htptable-format-cell (col-format row-data-list)
  (cl-assert (htptable-col-format-p col-format))
  (let* ((formatter (htptable-col-format-data-formatter col-format))
         (width (htptable-col-format-width col-format))
         (orig-str (funcall formatter row-data-list)))
    (truncate-string-to-width (format (format "%%-%ds" width)
                                      orig-str)
                              width)))


;;; table
(cl-defstruct (htptable-table (:constructor htptable-make-table--internal))
  col-format-list
  row-data-list)

(cl-defun htptable-make-table (&key col-format-list row-data-list)
  (htptable-make-table--internal :col-format-list col-format-list
                                 :row-data-list row-data-list))

(defun htptable-table-to-string (table)
  (cl-assert (htptable-table-p table))
  (let ((col-format-list (htptable-table-col-formats table))
        (row-data-list (htptable-table-row-data table))
        (result ""))
    ;; add header
    (dolist (col-format col-format-list)
      (setq result
            (concat result
                    (htptable-format-header col-format)
                    " ")))
    (add-face-text-property 0 (length result)
                            'htptable-header-face
                            nil
                            result)
    (setq result
          (concat result "\n"))
    ;; add rows
    (dolist (row-data row-data-list)
      (dolist (col-format col-format-list)
        (setq result
              (concat result
                      (htptable-format-cell col-format row-data)
                      " ")))
      (setq result
            (concat result "\n")))
    result))


(provide 'htprofile-table)
;;; htprofile-table.el ends here
