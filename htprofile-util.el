;;; htprofile-util.el --- util                       -*- lexical-binding: t; -*-

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

;; (defun htprofile-format (obj str-len)
;;   (truncate-string-to-width (format (format "%%-%ds" str-len)
;;                                     obj)
;;                             str-len))
(defvar htprofile-float-format 'msec
  "Specifies the format to show float number (elapsed time)
The value should be one of the following:
- sec (symbol)
- msec (symbol)
- (:width NUM :precision NUM :multiplier NUM) (plist)")
(defun htprofile-get-float-format ()
  (cond
   ((eq htprofile-float-format 'sec)
    (list :width 9
          :precision 6
          :multiplier 1))
   ((eq htprofile-float-format 'msec)
    (list :width 6
          :precision 0
          :multiplier 1000))
   ((and (listp htprofile-float-format)
         (plist-get htprofile-float-format :width)
         (plist-get htprofile-float-format :precision)
         (plist-get htprofile-float-format :multiplier))
    htprofile-float-format)
   (t
    (error "Invalid value: htprofile-float-format"))))
(defun htprofile-get-float-format-description ()
  (cond
   ((eq htprofile-float-format 'sec)
    "second")
   ((eq htprofile-float-format 'msec)
    "millisecond")
   (t
    "user-defined format")))
(defun htprofile-get-float-width ()
  (plist-get (htprofile-get-float-format) :width))
(defun htprofile-get-float-precision ()
  (plist-get (htprofile-get-float-format) :precision))
(defun htprofile-get-float-multiplier ()
  (plist-get (htprofile-get-float-format) :multiplier))
(defun htprofile-float-to-str (float-number)
  (let* ((multiplied (* float-number
                        (htprofile-get-float-multiplier))))
    (when (eq (htprofile-get-float-precision) 0)
      (setq multiplied (floor multiplied)))
    (truncate-string-to-width (format (format "%%%d.%df"
                                              (htprofile-get-float-width)
                                              (htprofile-get-float-precision))
                                      multiplied)
                              (htprofile-get-float-width))))

(defun htprofile--filter-list (list &optional beg end filter)
  (let ((len (length list))
        data-list)
    (if beg
        (setq beg (max beg 0))
      (setq beg 0))
    (if end
        (setq end (min end len))
      (setq end len))
    (setq data-list (cl-subseq list
                               (- len end)
                               (- len beg)))
    (if filter
        (seq-filter filter data-list)
      data-list)))

(provide 'htprofile-util)
;;; htprofile-util.el ends here
