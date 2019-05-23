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

(cl-defstruct (htpviewer-viewer (:constructor htpviewer-make-viewer--internal))
  buffer-name)

(cl-defun htpviewer-make-viewer (&key buffer-name)
  (cl-check-type buffer-name string)
  ;; (cl-assert (htptable-table-p table))
  (htpviewer-make-viewer--internal :buffer-name buffer-name))

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
    (let (;; (table (htpviewer-viewer-table viewer))
          (inhibit-read-only t))
      (insert "\n")
      (insert (htptable-table-to-string table)))
    ;; (goto-char (point-min))
    ;; (display-buffer (current-buffer))
    ))

(defun htpviewer-show-viewer (viewer)
  (with-current-buffer (htpviewer-get-buffer viewer)
    (goto-char (point-min))
    (display-buffer (current-buffer))))


(provide 'htprofile-viewer)
;;; htprofile-viewer.el ends here
