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
  header width data-formatter align)

(cl-defun htptable-make-col-format (&key header width data-formatter align)
  (cl-check-type header string) ;; (cl-assert (stringp header))
  (cl-check-type width (or integer null)) ;; (cl-assert (or (integerp width) (null width)))
  (cl-check-type data-formatter function) ;; (cl-assert (functionp data-formatter))
  (cl-assert (cl-destructuring-bind (beg . end) (func-arity data-formatter)
               (and (<= beg 1)
                    (or (eq end 'many)
                        (<= 1 end))))
             nil "data-formatter must be a function with 1 argument")
  (unless align
    (setq align 'left))
  (cl-check-type align symbol) ;; (cl-assert (and (symbolp align) (memq align '(left right))))
  (cl-assert (memq align '(left right)))
  (htptable-make-col-format--internal :header header
                                      :width width
                                      :data-formatter data-formatter
                                      :align align))

(defvar htptable-truncate-ellipsis
  ".")

(defface htptable-truncate-ellipsis-face
  '((t :inherit font-lock-comment-face))
  "face for ellipsis in table")

(defun htptable-ellipsis-pressed (button)
  (message (button-get button 'htprofile-truncated-string)))

(defun htptable-normalize-string (str width align &optional ellipsis-face)
  (when (eq align 'right)
    (setq str (format (format "%%%ds" width)
                      str)))            ; padding
  (if (null width)
      str
    (let* ((str-len (length str))
           (truncated-str (truncate-string-to-width str width nil
                                                    32 ;; charcode of space
                                                    htptable-truncate-ellipsis)))
      (when (> str-len width)
        (add-text-properties (1- width) width
                             (list 'action 'htptable-ellipsis-pressed
                                   'button t
                                   'category 'default-button
                                   'follow-link t
                                   'htprofile-truncated-string str)
                             truncated-str)
        (when ellipsis-face
          (add-face-text-property (- width (length htptable-truncate-ellipsis)) width
                                  ellipsis-face nil
                                  truncated-str)))

      truncated-str)))

(defface htptable-header-face
  '((t :inverse-video t))
  "face for header")

(defun htptable-format-header (col-format)
  (let* ((width (htptable-col-format-width col-format))
         (align (htptable-col-format-align col-format))
         (orig-header (htptable-col-format-header col-format)))
    (htptable-normalize-string orig-header width align)
    ))

(defun htptable-format-cell (col-format row-data-list)
  (cl-assert (htptable-col-format-p col-format))
  (let* ((formatter (htptable-col-format-data-formatter col-format))
         (width (htptable-col-format-width col-format))
         (align (htptable-col-format-align col-format))
         (orig-str (funcall formatter row-data-list)))
    (htptable-normalize-string orig-str width align ;; 'htptable-truncate-ellipsis-face
                               )))


;;; table
(cl-defstruct (htptable-table (:constructor htptable-make-table--internal))
  col-format-list
  row-data-list)

(cl-defun htptable-make-table (&key col-format-list row-data-list)
  (htptable-make-table--internal :col-format-list col-format-list
                                 :row-data-list row-data-list))

(defun htptable-table-to-string (table)
  "not recommended to use (slow)"
  ;; 文字列結合は重い？O(N^2)?
  (cl-assert (htptable-table-p table))
  (let ((col-format-list (htptable-table-col-format-list table))
        (row-data-list (htptable-table-row-data-list table))
        (result ""))
    ;; add header
    (dolist (col-format col-format-list)
      (setq result
            (concat result
                    (htptable-format-header col-format)
                    " ")))
    (setq result
          (concat result "\n"))
    (add-face-text-property 0 (length result)
                            'htptable-header-face
                            nil
                            result)
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

(defun htptable-insert-table (table)
  "Same result as(insert (htptable-table-to-string table)), but much faster"
  (cl-assert (htptable-table-p table))
  (let ((col-format-list (htptable-table-col-format-list table))
        (row-data-list (htptable-table-row-data-list table))
        (header-start (point)))
    ;; add header
    (dolist (col-format col-format-list)
      (insert (format "%s " (htptable-format-header col-format))))
    (insert "\n")
    (add-face-text-property header-start (point) 'htptable-header-face)
    ;; add rows
    (dolist (row-data row-data-list)
      (dolist (col-format col-format-list)
        (insert (format "%s " (htptable-format-cell col-format row-data))))
      (insert "\n"))))


(provide 'htprofile-table)
;;; htprofile-table.el ends here
