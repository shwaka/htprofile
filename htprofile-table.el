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

(cl-defstruct (htptable-col-format (:constructor htptable-make-col-format--internal))
  header width data-formatter truncation)

(cl-defun htptable-make-col-format (&key header width data-formatter truncation)
  (htptable-make-col-format--internal :header header
                                      :width width
                                      :data-formatter data-formatter
                                      :truncation truncation))

(defun htptable-format-header (col-format)
  (let* ((orig-header (htptable-col-format-header))
         (width (htptable-col-format-width)))
    (truncate-string-to-width (format (format "%%-%ds" width)
                                      orig-header)
                              width)))

(defun htptable-format-cell (col-format row-data)
  (let* ((formatter (htptable-col-format-data-formatter col-format))
         (width (htptable-col-format-width col-format))
         (orig-str (funcall formatter row-data)))
    (truncate-string-to-width (format (format "%%-%ds" width)
                                      orig-str)
                              width)))

(cl-defstruct (htptable-table (:constructor htptable-make-table--internal))
  col-formats
  row-data)

(cl-defun htptable-make-table (&key col-formats row-data)
  (htptable-make-table--internal :col-formats col-formats
                                 :row-data row-data))


(provide 'htprofile-table)
;;; htprofile-table.el ends here
