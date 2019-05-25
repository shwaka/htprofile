;;; htprofile.el --- Profile pre/post-command-hook and (idle-)timer  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shun Wakatsuki

;; Author: Shun Wakatsuki <shun.wakatsuki@gmail.com>
;; Keywords: convenience
;; Package-Requires: (cl-lib seq)

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
;; This package may be useful when your Emacs become heavy and you don't
;; know which function is heavy.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'htprofile-widgets)
(require 'htprofile-viewer)
(require 'htprofile-table)
(require 'htprofile-util)

(defvar htprofile-data-list ()
  "save data to this variable")
(defvar htprofile--advice-list ()
  "list of advice functions")
(defvar htprofile-func-name-format
  "%S"
  "string which is used to format func-name")

;;; timer
(defun htprofile-advice:timer-event-handler (orig-func &rest args)
  (let* ((timer (car args))
         (func (timer--function timer))
         (type (if (timer--idle-delay timer)
                   'idle-timer
                 'timer))
         (idle-time (if (eq type 'idle-timer)
                        (float-time (list (timer--high-seconds timer)
                                          (timer--low-seconds timer)
                                          (timer--usecs timer)))
                      nil)))
    (htprofile-profile-func orig-func args type (format htprofile-func-name-format
                                                        func)
                            :idle-time idle-time)))
(defun htprofile-profile-timer ()
  (let ((symb 'timer-event-handler)
        (func 'htprofile-advice:timer-event-handler))
    (advice-add symb :around func)
    (add-to-list 'htprofile--advice-list (cons symb func))))

;;; hooks
;;; buffer-list-update-hook とかは出てくるけど，
;;; 肝心の pre(post)-command-hook は出てこない
;; (defun around:run-hooks (orig-func &rest args)
;;   (apply orig-func args))
;; (advice-add 'run-hooks :around 'around:run-hooks)
(defun htprofile-profile-hook (hook-var)
  (dolist (func (eval hook-var))
    (let ((advice-name (intern (format "htprofile-advice:%s" func))))
      (eval `(defun ,advice-name (orig-func &rest args)
               "advice added in order to profile"
               (htprofile-profile-func orig-func args (quote ,hook-var) (quote ,func))))
      (advice-add func :around advice-name)
      (add-to-list 'htprofile--advice-list (cons func advice-name)))))


;;; profiler
(defvar htprofile-remove-newline t
  "When non-nil, remove newline in function names (recommended)")
(defun htprofile-maybe-remove-newline (str)
  (when htprofile-remove-newline
    (replace-regexp-in-string "\n" " " str)))
(defvar htprofile--data-id -1)
(cl-defstruct (htprofile-data (:constructor make-htprofile-data--internal))
  "func-name must be a string"
  type func-name elapsed-time idle-time id current-time)
(cl-defun make-htprofile-data (&key type func-name elapsed-time idle-time)
  (let ((func-name-str (if (stringp func-name)
                           func-name
                         (format htprofile-func-name-format
                                 func-name))))
    (setq htprofile--data-id (1+ htprofile--data-id))
    (make-htprofile-data--internal :type type
                                   :func-name func-name-str
                                   :elapsed-time elapsed-time
                                   :idle-time idle-time
                                   :id htprofile--data-id
                                   :current-time (current-time))))
(defvar htprofile-data-id-digit 6)

(cl-defstruct htprofile-key
  type func-name idle-time)
(defun htprofile-data-to-key (data)
  (make-htprofile-key :type (htprofile-data-type data)
                      :func-name (htprofile-data-func-name data)
                      :idle-time (htprofile-data-idle-time data)))
(cl-defun htprofile-profile-func (func args type func-name &key idle-time)
  (let ((start-time (current-time))
        end-time
        elapsed-time)
    (apply func args)
    (setq end-time (current-time)
          elapsed-time (time-subtract end-time start-time))
    (push (make-htprofile-data :type type
                               :func-name func-name
                               :elapsed-time elapsed-time
                               :idle-time idle-time)
          htprofile-data-list)))

;;; get data
(defvar htprofile-data-filter-function 'htprofile-default-filter-function)
(defun htprofile-get-data-list (&optional beg end filter)
  (htprofile--filter-list htprofile-data-list beg end filter)
  ;; (let ((len (length htprofile-data-list))
  ;;       data-list)
  ;;   (if beg
  ;;       (setq beg (max beg 0))
  ;;     (setq beg 0))
  ;;   (if end
  ;;       (setq end (min end len))
  ;;     (setq end len))
  ;;   (setq data-list (cl-subseq htprofile-data-list
  ;;                              (- len end)
  ;;                              (- len beg)))
  ;;   (if filter
  ;;       (seq-filter filter data-list)
  ;;     data-list))
  )

(defun htprofile-data-list-length ()
  (length htprofile-data-list))


;;; compute statistics
(cl-defstruct htprofile-stat
  type idle-time func len
  total-time max-time average-time)
(defun htprofile-split-data-list-by-keys ()
  (let (data-list-alist)
    (dolist (data (htprofile-get-data-list nil nil htprofile-data-filter-function))
      (let ((key (htprofile-data-to-key data)))
        (if (assoc key data-list-alist)
            (push data (cdr (assoc key data-list-alist)))
          (push (cons key (list data)) data-list-alist))))
    data-list-alist))
(defun htprofile-compute-summary (key data-list)
  (let* ((type (htprofile-key-type key))
         (func-name (htprofile-key-func-name key))
         (idle-time (htprofile-key-idle-time key))
         (len (length data-list))
         (total-time 0)
         (max-time 0)
         total-time-float
         max-time-float
         average-time-float)
    (dolist (data data-list)
      (let ((time (htprofile-data-elapsed-time data)))
        (setq total-time (time-add total-time time))
        (when (time-less-p max-time time)
          (setq max-time time))))
    (setq total-time-float (float-time total-time)
          max-time-float (float-time max-time)
          average-time-float (/ total-time-float len))
    (make-htprofile-stat :type type :func func-name :len len :idle-time idle-time
                         :total-time total-time-float
                         :max-time max-time-float
                         :average-time average-time-float)))

(defun htprofile-get-type-str (type idle-time)
  (if idle-time
      (format "%s:%.2f"
              type
              idle-time)
    (symbol-name type)))
(defvar htprofile-stat-col-format-list
  (list
   (htptable-make-col-format
    :header "type" :width 20 :align 'left
    :data-formatter (lambda (data) (htprofile-get-type-str (htprofile-stat-type data)
                                                           (htprofile-stat-idle-time data))))
   (htptable-make-col-format
    :header "count" :width 5 :align 'right
    :data-formatter (lambda (data) (htprofile-stat-len data)))
   (htptable-make-col-format
    :header "total" :width (htprofile-get-float-width) :align 'right
    :data-formatter (lambda (data) (htprofile-float-to-str (htprofile-stat-total-time data))))
   (htptable-make-col-format
    :header "max" :width (htprofile-get-float-width) :align 'right
    :data-formatter (lambda (data) (htprofile-float-to-str (htprofile-stat-max-time data))))
   (htptable-make-col-format
    :header "ave" :width (htprofile-get-float-width) :align 'right
    :data-formatter (lambda (data) (htprofile-float-to-str (htprofile-stat-average-time data))))
   (htptable-make-col-format
    :header "func" :width nil
    :data-formatter (lambda (data) (htprofile-maybe-remove-newline (htprofile-stat-func data))))))


;;; interface
(defun htprofile-start (clean)
  "start profiling"
  (interactive
   (list (if htprofile-data-list
             (y-or-n-p "htprofile already has data. Clean data? (y/n): ")
           nil)))
  (when clean
    (setq htprofile-data-list nil))
  (htprofile-profile-hook 'pre-command-hook)
  (htprofile-profile-hook 'post-command-hook)
  (htprofile-profile-timer)
  (when (called-interactively-p 'interactive)
    (message "htprofile started. Use htprofile-show-statistics or htprofile-show-log to see the results")))
(defun htprofile-stop ()
  (interactive)
  (dolist (adv htprofile--advice-list)
    (let ((symb (car adv))
          (func (cdr adv)))
      (advice-remove symb func)))
  (when (called-interactively-p 'interactive)
    (message "htprofile stopped. Use htprofile-start to restart again.")))

(defvar htprofile-sort-by 'max-time)
(defvar htprofile-statistics-buffer "*htprofile-stat*")
(defvar htprofile-stat-variable-list
  '((:symbol htprofile-sort-by :type symbol :description "sort by" :candidates (total-time max-time average-time)))
  "list of variables which are used in log")
(defun htprofile-update-statistics ()
  (let ((stat-list ())
        (sort-key-func (intern (concat "htprofile-stat-" (symbol-name htprofile-sort-by)))))
    (dolist (data-list-with-key (htprofile-split-data-list-by-keys))
      (let ((key (car data-list-with-key))
            (data-list (cdr data-list-with-key)))
        (push (htprofile-compute-summary key data-list) stat-list)))
    (setq stat-list (cl-sort stat-list '> :key sort-key-func))
    (let* ((viewer (htpviewer-make-viewer :buffer-name htprofile-statistics-buffer
                                          :variable-list htprofile-stat-variable-list
                                          :update-func 'htprofile-update-statistics))
           (table (htptable-make-table
                   :col-format-list htprofile-stat-col-format-list
                   :row-data-list stat-list)))
      (htpviewer-update-viewer viewer table)
      (htpviewer-show-viewer viewer))))
(defun htprofile-show-statistics ()
  "show data in a buffer *htprofile-stat*"
  (interactive)
  (htprofile-update-statistics)
  (with-current-buffer (get-buffer htprofile-statistics-buffer)
    ;; (setq htprofile--buffer-update-function 'htprofile-update-statistics)
    (goto-char (point-min))
    (display-buffer (current-buffer))))
(defvar htprofile-max-log
  1000
  "The number of data which are shown by `htprofile-show-log'")
(defvar htprofile--show-log-from)
(defvar htprofile--show-log-to)

(defvar htprofile-log-buffer "*htprofile-log*")
(defvar htprofile-log-col-format-list
  (list
   (htptable-make-col-format
    :header "id" :width htprofile-data-id-digit :align 'right
    :data-formatter (lambda (data) (htprofile-data-id data)))
   (htptable-make-col-format
    :header "current" :width 9 :align 'left
    :data-formatter (lambda (data) (format-time-string
                                    "%H:%M:%S"
                                    (htprofile-data-current-time data))))
   (htptable-make-col-format
    :header "type" :width 20 :align 'left
    :data-formatter (lambda (data) (htprofile-get-type-str
                                    (htprofile-data-type data)
                                    (htprofile-data-idle-time data))))
   (htptable-make-col-format
    :header "elapse" :width (htprofile-get-float-width) :align 'left
    :data-formatter (lambda (data) (htprofile-float-to-str
                                    (float-time
                                     (htprofile-data-elapsed-time data)))))
   (htptable-make-col-format
    :header "func" :width nil
    :data-formatter (lambda (data) (htprofile-maybe-remove-newline
                                    (htprofile-data-func-name data)))))
  "list of col-format for log")
(defvar htprofile-log-variable-list
  '((:symbol htprofile--show-log-from :type integer :description "show log from")
    (:symbol htprofile--show-log-to :type integer :description "show log up to")
    (:symbol htprofile-min-elapsed-time :type integer :description "minimum elapsed time"))
  "list of variables which are used in log")
(defun htprofile-update-log ()
  (let* ((num-data (length (htprofile-get-data-list)))
         (message (format "Showing %d--%d of %d data"
                          htprofile--show-log-from
                          htprofile--show-log-to
                          num-data))
         (viewer (htpviewer-make-viewer :buffer-name htprofile-log-buffer
                                        :variable-list htprofile-log-variable-list
                                        :update-func 'htprofile-update-log
                                        :message message))
         (table (htptable-make-table
                 :col-format-list htprofile-log-col-format-list
                 :row-data-list (htprofile-get-data-list htprofile--show-log-from
                                                         htprofile--show-log-to
                                                         htprofile-data-filter-function))))
    (htpviewer-update-viewer viewer table)
    (htpviewer-show-viewer viewer)))
(defun htprofile-show-log ()
  "show data in a buffer *htprofile-log*"
  (interactive)
  (let ((len (htprofile-data-list-length)))
    (setq htprofile--show-log-from (max 0 (- len htprofile-max-log))
          htprofile--show-log-to len))
  (htprofile-update-log)
  ;; (with-current-buffer (get-buffer htprofile-log-buffer)
  ;;   ;; (setq htprofile--buffer-update-function 'htprofile-update-log)
  ;;   (goto-char (point-min))
  ;;   (display-buffer (current-buffer)))
  )

;;; filter function
(defvar htprofile-min-elapsed-time 0
  "Time (millisecond) used in `htprofile-default-filter-function'.")

(defun htprofile-default-filter-function (data)
  (let* ((time (htprofile-data-elapsed-time data))
         (time-float (float-time time)))
    (>= (* 1000 time-float) htprofile-min-elapsed-time)))

(provide 'htprofile)
;;; htprofile.el ends here
