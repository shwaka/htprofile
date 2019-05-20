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
  header width data-formatter)

(cl-defun htptable-make-col-format (&key header width data-formatter)
  (cl-assert (stringp header))
  (cl-assert (or (integerp width)
                 (null width)))
  (cl-assert (functionp data-formatter))
  (htptable-make-col-format--internal :header header
                                      :width width
                                      :data-formatter data-formatter))

(defvar htptable-truncate-ellipsis
  ".")

(defface htptable-truncate-ellipsis-face
  '((t :inherit font-lock-comment-face))
  "face for ellipsis in table")

(defun htptable-normalize-string (str width &optional ellipsis-face)
  (if (null width)
      str
    (let* ((str-len (length str))
           (truncated-str (truncate-string-to-width str width nil
                                                    32 ;; charcode of space
                                                    htptable-truncate-ellipsis)))
      (when (and ellipsis-face
                 (> str-len width))
        (add-face-text-property (- width (length htptable-truncate-ellipsis)) width
                                ellipsis-face nil
                                truncated-str))
      truncated-str))                       ; only padding
  ;; (if (> (length str) width)
  ;;     (concat (substring str 0 (- width (length htptable-truncate-indicator)))
  ;;             htptable-truncate-indicator))
  ;; (truncate-string-to-width (format (format "%%-%ds" width)
  ;;                                   str)
  ;;                           width)
  )

(defface htptable-header-face
  '((t :inverse-video t))
  "face for header")

(defun htptable-format-header (col-format)
  (let* ((width (htptable-col-format-width col-format))
         (orig-header (htptable-col-format-header col-format))
         ;; (header (if (htptable-col-format-truncation col-format)
         ;;             (htptable-normalize-string orig-header width)
         ;;           orig-header)
         ;;         ;; (truncate-string-to-width (format (format "%%-%ds" width)
         ;;         ;;                                   orig-header)
         ;;         ;;                           width)
         ;;         )
         )
    (htptable-normalize-string orig-header width)
    ;; (propertize header 'face 'htptable-header-face)
    ))

(defun htptable-format-cell (col-format row-data-list)
  (cl-assert (htptable-col-format-p col-format))
  (let* ((formatter (htptable-col-format-data-formatter col-format))
         (width (htptable-col-format-width col-format))
         (orig-str (funcall formatter row-data-list)))
    (htptable-normalize-string orig-str width 'htptable-truncate-ellipsis-face)
    ;; (htptable-normalize-string orig-str width 'htptable-truncate-ellipsis-face)
    ;; (truncate-string-to-width (format (format "%%-%ds" width)
    ;;                                   orig-str)
    ;;                           width)
    ))


;;; table
(cl-defstruct (htptable-table (:constructor htptable-make-table--internal))
  col-format-list
  row-data-list)

(cl-defun htptable-make-table (&key col-format-list row-data-list)
  (htptable-make-table--internal :col-format-list col-format-list
                                 :row-data-list row-data-list))

(defun htptable-table-to-string (table)
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


(provide 'htprofile-table)
;;; htprofile-table.el ends here
