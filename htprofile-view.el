;;; htprofile-view.el --- formatter for htprofile    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shun

;; Author: Shun Wakatsuki <shun.wakatsuki@gmail.com>
;; Keywords: convenience
;; Package-Requires: (cl-lib)

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

(cl-defstruct (htpv-format (:constructor make-htpv-format--internal))
  length align header func)
(cl-defun make-htpv-format (&key length align header func)
  "validate members"
  (assert (and (integerp length)
               (> length 0)))
  (assert (memq align '(left right)))
  (assert (stringp header))
  (assert (functionp func))
  (make-htpv-format--internal :length length
                              :align align
                              :header header
                              :func func))

(defun htpv-truncate-string (str length align)
  (let* ((align-str (cl-case align
                      ('left "-")
                      ('right "")
                      ('otherwise (error "invalid align parameter"))))
         (percent-seq (format "%%%ss" (format "%s%d" align-str length))))
    (truncate-string-to-width (format percent-seq str)
                              length)))
(defun htpv-get-header (format-list)
  (let ((header-whole ""))
    (dolist (format format-list)
      (let* ((length (htpv-format-length format))
             (align (htpv-format-align format))
             (header (htpv-format-header format)))
        (setq header-whole (concat header-whole
                                   (htpv-truncate-string header length align)))))
    header-whole))

(defun htpv-get-row (format-list data)
  (let ((row ""))
    (dolist (format format-list)
      (let* ((length (htpv-format-length format))
             (align (htpv-format-align format))
             (func (htpv-format-func format)))
        (setq row (concat row
                          (htpv-truncate-string (funcall func data) length align)))))
    row))

(defun htpv-get-clean-buffer (buffer-name)
  "copied from `htprofile-get-clean-buffer'"
  (let ((inhibit-read-only t))
    (if (get-buffer buffer-name)
        (with-current-buffer (get-buffer buffer-name)
          (erase-buffer)
          (current-buffer))
      (with-current-buffer (get-buffer-create buffer-name)
        (setq truncate-lines t
              buffer-read-only t)
        (current-buffer)))))

;;; test
(defun htpv-test-func (data)
  (format "%s" data))
(defun htpv-test ()
  (interactive)
  (let* ((f1 (make-htpv-format :length 5
                               :align 'right
                               :header "hoge"
                               :func #'htpv-test-func))
         (f2 (make-htpv-format :length 4
                               :align 'left
                               :header "a"
                               :func #'htpv-test-func))
         (fl (list f1 f2)))
    (with-current-buffer (htpv-get-clean-buffer "*htpv-test*")
      (let ((inhibit-read-only t))
        (insert (htpv-get-header fl))
        (insert "\n")
        (insert (htpv-get-row fl "foo")))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'htprofile-view)
;;; htprofile-view.el ends here
