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
(require 'htprofile-util)

;; (defun htprofile-advice:company-call-backend (orig-func &rest args)
;;   (let ((start-time (current-time))
;;         end-time
;;         elapsed-time)
;;     (apply orig-func args)
;;     (setq end-time (current-time)
;;           elapsed-time (time-subtract end-time start-time))
;;     (my-message "%S" (list args (float-time elapsed-time)))))
;; (advice-add 'company-call-backend :around 'htprofile-advice:company-call-backend)

(defvar htprofile-company--backend-advice-list ()
  "list of advice functions")
(defvar htprofile-company-backend-data-list ()
  "save data to this variable")
(defvar htprofile-company-backend-data-filter-function 'htprofile-company-default-backend-data-filter)
(defvar htprofile-company-backend-min-elapsed-time 0
  "Time (millisecond) used in `htprofile-company-default-backend-data-filter'.")
(defvar htprofile-company-backend-name-regexp
  "."
  "Regular expression used in `htprofile-company-default-backend-data-filter'.")
(defun htprofile-company-get-backend-data-list (&optional beg end filter)
  (htprofile--filter-list htprofile-company-backend-data-list
                          beg end filter))
(defun htprofile-company-default-backend-data-filter (data)
  (and (let* ((time (htprofile-company-backend-data-elapsed-time data))
              (time-float (float-time time)))
         (>= (* 1000 time-float) htprofile-company-backend-min-elapsed-time))
       (let* ((backend-name-symbol (htprofile-company-backend-data-backend-name data))
              (backend-name (symbol-name backend-name-symbol)))
         (string-match-p htprofile-company-backend-name-regexp
                         backend-name))))

(defun htprofile-company-profile-backends ()
  (interactive)
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
      (eval `(defun ,advice-name (orig-func &rest command-args)
               "advice added in order to profile"
               (htprofile-company--run-backend-with-profile orig-func command-args (quote ,backend))))
      (advice-add backend :around advice-name)
      (add-to-list 'htprofile-company--backend-advice-list (cons backend advice-name)))))
(defun htprofile-company--run-backend-with-profile (backend command-args backend-name)
  "htprofile-profile-func を参考"
  (let* ((start-time (current-time))
         (result (apply backend command-args))
         (end-time (current-time))
         (elapsed-time (time-subtract end-time start-time))
         async)
    (when (and (listp result)
               (eq (car result) :async))
      ;; when async
      (setq async 'start)
      (setq result (cons :async
                         (htprofile-company--advise-async-fetcher (cdr result)
                                                            backend-name
                                                            command-args
                                                            start-time))))
    ;; (setq end-time (current-time)
    ;;       elapsed-time (time-subtract end-time start-time))
    (push (htprofile-company-make-backend-data :backend-name backend-name
                                       :command-args command-args
                                       :elapsed-time elapsed-time
                                       :async async)
          htprofile-company-backend-data-list)
    result))
(defun htprofile-company--advise-async-fetcher (fetcher backend-name command-args start-time)
  (lambda (callback)
    (let ((callback-with-hook (htprofile-company--advise-async-callback callback
                                                                  backend-name
                                                                  command-args
                                                                  start-time)))
      (funcall fetcher callback-with-hook))))
(defun htprofile-company--advise-async-callback (callback backend-name command-args start-time)
  (lambda (return-value)
    (push (htprofile-company-make-backend-data :backend-name backend-name
                                       :command-args command-args
                                       :elapsed-time (time-subtract (current-time)
                                                                    start-time)
                                       :async 'end)
          htprofile-company-backend-data-list)
    (funcall callback return-value)))

;;; struct
(defvar htprofile-company--data-id -1)
(cl-defstruct (htprofile-company-backend-data (:constructor htprofile-company-make-backend-data--internal))
  "data for each call of company backend"
  backend-name command args elapsed-time id current-time async)
(cl-defun htprofile-company-make-backend-data (&key backend-name command-args elapsed-time async)
  (cl-check-type backend-name symbol) ;; (assert (symbolp backend-name))
  (cl-check-type elapsed-time list)  ;; (assert (listp elapsed-time))
  ;; (cl-check-type args list)
  (cl-check-type async symbol)
  (setq htprofile-company--data-id (1+ htprofile-company--data-id))
  (let ((command (car command-args))
        (args (cdr command-args)))
    (htprofile-company-make-backend-data--internal :backend-name backend-name
                                           :command command
                                           :args args
                                           :elapsed-time elapsed-time
                                           :id htprofile-company--data-id
                                           :current-time (current-time)
                                           :async async)))

;;; user interface
(defvar htprofile-company-backend-log-buffer
  "*htprofile-company-backend-log*")
(defvar htprofile-company-abbrev-backend-name
  t
  "company-capf -> -capf")
(defun htprofile-company-get-backend-name (data)
  "return the name of backend as a string (possibly abbreviate)"
  (let* ((orig-symbol (htprofile-company-backend-data-backend-name data))
         (orig-str (symbol-name orig-symbol)))
    (if htprofile-company-abbrev-backend-name
        (replace-regexp-in-string (rx bol "company-") "-" orig-str)
      orig-str)))
(defun htprofile-company-format-args-from-backend-data (data)
  (let ((args (htprofile-company-backend-data-args data)))
    (cond
     ((null args) (propertize "<no args>"
                              'face 'font-lock-comment-face))
     ((eq (length args) 1)
      (let ((arg (car args)))
        (if (stringp arg)
            (format "\"%s\"" arg)
          (format "%S" arg))))
     (t (format "%d args: %S" (length args) args)))))
(defun htprofile-company-format-async-from-backend-data (data)
  (let ((async (htprofile-company-backend-data-async data)))
    (if async
        (format "%s" async)
      "")))
(defvar htprofile-company-backend-log-col-format-list
  (list
   (htptable-make-col-format
    :header "id" :width 'max :align 'right
    :data-formatter (lambda (data) (format "%s" (htprofile-company-backend-data-id data))))
   (htptable-make-col-format
    :header "current" :width 9 :align 'left
    :data-formatter (lambda (data) (format-time-string
                                    "%H:%M:%S"
                                    (htprofile-company-backend-data-current-time data))))
   (htptable-make-col-format
    :header "backend" :width 'max :align 'left
    :data-formatter 'htprofile-company-get-backend-name)
   (htptable-make-col-format
    :header "elapse" :width 6 :align 'left
    :data-formatter (lambda (data)
                      (format "%s" (htprofile-float-to-str
                                    (float-time (htprofile-company-backend-data-elapsed-time data))))))
   (htptable-make-col-format
    :header "async" :width 5 :align 'left
    :data-formatter 'htprofile-company-format-async-from-backend-data)
   (htptable-make-col-format
    :header "command" :width 'max
    :data-formatter (lambda (data) (format "%s" (htprofile-company-backend-data-command data))))
   (htptable-make-col-format
    :header "args" :width nil
    :data-formatter 'htprofile-company-format-args-from-backend-data)))


(defvar htprofile-company-backend-log-variable-list
  '((:symbol htprofile-company-backend-min-elapsed-time :type integer :description "minimum elapsed time")
    (:symbol htprofile-company-abbrev-backend-name :type symbol :description "abbreviate backend name" :candidates (t nil))
    (:symbol htprofile-company-backend-name-regexp :type string :description "backend name regexp")))

(defun htprofile-company-update-backend-log ()
  (let* ((message (format "%s logs" (length (htprofile-company-get-backend-data-list))))
         (viewer (htpviewer-make-viewer :buffer-name htprofile-company-backend-log-buffer
                                        :variable-list htprofile-company-backend-log-variable-list
                                        :update-func 'htprofile-company-update-backend-log
                                        :message message))
         (table (htptable-make-table
                 :col-format-list htprofile-company-backend-log-col-format-list
                 :row-data-list (htprofile-company-get-backend-data-list
                                 nil nil htprofile-company-backend-data-filter-function))))
    (htpviewer-update-viewer viewer table)
    (goto-char (point-min))
    ;; (htpviewer-show-viewer viewer)
    viewer))

(defun htprofile-company-show-backend-log ()
  (interactive)
  (let ((viewer (htprofile-company-update-backend-log)))
    (htpviewer-show-viewer viewer)))

(provide 'htprofile-company)
;;; htprofile-company.el ends here
