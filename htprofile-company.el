;;; htprofile-company.el --- profiler for company    -*- lexical-binding: t; -*-

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

;; work in progress

;;; Code:

(require 'cl-lib)
(require 'htprofile-widgets)
(require 'htprofile-viewer)
(require 'htprofile-table)

;; (defun htprofile-advice:company-call-backend (orig-func &rest args)
;;   (let ((start-time (current-time))
;;         end-time
;;         elapsed-time)
;;     (apply orig-func args)
;;     (setq end-time (current-time)
;;           elapsed-time (time-subtract end-time start-time))
;;     (my-message "%S" (list args (float-time elapsed-time)))))
;; (advice-add 'company-call-backend :around 'htprofile-advice:company-call-backend)

(defvar htprofile-company--advice-list ()
  "list of advice functions")
(defvar htprofile-company-data-list ()
  "save data to this variable")

(defun htprofile-company-profile-backends ()
  (let (backend-list)
    ;; flatten company-backends (ignoring :with)
    (dolist (backend company-backends)
      (cond
       ((symbolp backend) (add-to-list 'backend-list backend))
       ((listp backend)
        (dolist (b backend)
          (unless (keywordp b)
            (add-to-list 'backend-list b))))))
    (htprofile-company--profile-backends backend-list)))
(defun htprofile-company--profile-backends (backend-list)
  "htprofile-profile-hook を参考"
  (dolist (backend backend-list)
    (let ((advice-name (intern (format "htprofile-company-advice:%s" backend))))
      (eval `(defun ,advice-name (orig-func &rest args)
               "advice added in order to profile"
               (htprofile-company--run-backend-with-profile orig-func args (quote ,backend))))
      (advice-add backend :around advice-name)
      (add-to-list 'htprofile-company--advice-list (cons backend advice-name)))))
(defun htprofile-company--run-backend-with-profile (backend args backend-name)
  "htprofile-profile-func を参考"
  (let ((start-time (current-time))
        end-time
        elapsed-time)
    (prog1 (apply backend args)
      (setq end-time (current-time)
            elapsed-time (time-subtract end-time start-time))
      (push (htprofile-company-make-data :backend-name backend-name
                                         :args args
                                         :elapsed-time elapsed-time)
            htprofile-company-data-list))))

;;; struct
(cl-defstruct (htprofile-company-data (:constructor htprofile-company-make-data--internal))
  "data for each call of company backend"
  backend-name args elapsed-time)
(cl-defun htprofile-company-make-data (&key backend-name args elapsed-time)
  (assert (symbolp backend-name))
  (assert (listp elapsed-time))
  ;; (my-message "%S" (list backend-name args (float-time elapsed-time)))
  (htprofile-company-make-data--internal :backend-name backend-name
                                         :args args
                                         :elapsed-time elapsed-time))

;;; user interface
(defvar htprofile-company-log-buffer
  "*htprofile-company-log*")
(defvar htprofile-company-log-col-format-list
  (list
   (htptable-make-col-format
    :header "backend" :width 20 :align 'left
    :data-formatter (lambda (data) (format "%s" (htprofile-company-data-backend-name data))))
   (htptable-make-col-format
    :header "args" :width 30 :align 'left
    :data-formatter (lambda (data) (format "%s" (htprofile-company-data-args data))))
   (htptable-make-col-format
    :header "elapse" :width 6 :align 'left
    :data-formatter (lambda (data) (format "%s" (float-time (htprofile-company-data-elapsed-time data)))))))

(defun htprofile-company-update-log ()
  (interactive)
  (let* ((viewer (htpviewer-make-viewer :buffer-name htprofile-company-log-buffer
                                        :variable-list '()
                                        :update-func 'htprofile-company-update-log
                                        :message "companyyyyyy"))
         (table (htptable-make-table :col-format-list htprofile-company-log-col-format-list
                                     :row-data-list htprofile-company-data-list)))
    (htpviewer-update-viewer viewer table)
    (htpviewer-show-viewer viewer)))

;; (defun htprofile-company-show-log ()
;;   (interactive)
;;   (htprofile-company-update-log)
;;   (with-current-buffer (get-buffer htprofile-company-log-buffer)
;;     (goto-char (point-min))
;;     (display-buffer (current-buffer))))

(provide 'htprofile-company)
;;; htprofile-company.el ends here
