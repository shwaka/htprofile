(require 'cl-lib)
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
;;   (my-message "%S" args)
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
(defun htprofile-data-to-str (data)
  (format "%s %s  %-20s %s %s"
          (format (format "%%%dd" htprofile-data-id-digit)
                  (htprofile-data-id data))
          (format-time-string "%H:%M:%S" (htprofile-data-current-time data))
          (htprofile-get-type-str (htprofile-data-type data)
                                  (htprofile-data-idle-time data))
          (htprofile-float-to-str (float-time (htprofile-data-elapsed-time data)))
          (htprofile-maybe-remove-newline (htprofile-data-func-name data))))
(defun htprofile-get-data-header ()
  "get header"
  (let* ((description (format "elapsed time is shown as: %s\n"
                              (htprofile-get-float-format-description)))
         (header-plain (format "%s %s  %s %s %s\n"
                               (format (format "%%-%ds" htprofile-data-id-digit) "id")
                               (format "%-8s" "current")
                               (format "%-20s" "type")
                               (htprofile-format "elapsed" (htprofile-get-float-width))
                               "func"))
         (header (propertize header-plain
                             'face '(:inverse-video t))))
    (format "%s\n%s" description header)))

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
(defun htprofile-get-data-list (&optional beg end)
  (let ((len (length htprofile-data-list)))
    (if beg
        (setq beg (max beg 0))
      (setq beg 0))
    (if end
        (setq end (min end len))
      (setq end len))
    (cl-subseq htprofile-data-list
               (- len end)
               (- len beg))))


;;; compute statistics
(cl-defstruct htprofile-stat
  type idle-time func len
  total-time max-time average-time)
(defun htprofile-split-data-list-by-keys ()
  (let (data-list-alist)
    (dolist (data (htprofile-get-data-list))
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
(defun htprofile-format (obj str-len)
  (truncate-string-to-width (format (format "%%-%ds" str-len)
                                    obj)
                            str-len))
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
  (truncate-string-to-width (format (format "%%%d.%df"
                                            (htprofile-get-float-width)
                                            (htprofile-get-float-precision))
                                    (* float-number
                                       (htprofile-get-float-multiplier)))
                            (htprofile-get-float-width)))
(defun htprofile-get-type-str (type idle-time)
  (if idle-time
      (format "%s:%.2f"
              type
              idle-time)
    (symbol-name type)))
(defun htprofile-stat-to-str (stat)
  (let* ((type (htprofile-stat-type stat))
         (idle-time (htprofile-stat-idle-time stat)))
    (format "%s %s %s %s %s %s\n"
            (htprofile-format (htprofile-get-type-str type idle-time)
                              20)
            (format "%5s" (htprofile-stat-len stat))
            (htprofile-float-to-str (htprofile-stat-total-time stat))
            (htprofile-float-to-str (htprofile-stat-max-time stat))
            (htprofile-float-to-str (htprofile-stat-average-time stat))
            (htprofile-maybe-remove-newline (htprofile-stat-func stat)))))
(defun htprofile-get-stat-header ()
  "get header"
  (let* ((description (concat (format "total-time, max-time, average-time are shown as: %s\n"
                                      (htprofile-get-float-format-description))
                              (format "sort by: %s\n" htprofile-sort-by)))
         (header-plain (format "%s %s %s %s %s %s\n"
                               (format "%-20s" "type")
                               (format "%-5s" "count")
                               (htprofile-format "total" (htprofile-get-float-width))
                               (htprofile-format "max" (htprofile-get-float-width))
                               (htprofile-format "average" (htprofile-get-float-width))
                               "func"))
         (header (propertize header-plain
                             'face '(:inverse-video t))))
    (format "%s\n%s" description header)))


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

(defun htprofile-get-clean-buffer (buffer-name)
  (let ((inhibit-read-only t))
    (if (get-buffer buffer-name)
        (with-current-buffer (get-buffer buffer-name)
          (erase-buffer)
          (current-buffer))
      (with-current-buffer (get-buffer-create buffer-name)
        (setq truncate-lines t
              buffer-read-only t)
        (current-buffer)))))
(defvar htprofile-sort-by 'max-time)
(defvar htprofile-statistics-buffer "*htprofile-stat*")
(defun htprofile-update-statistics ()
  (with-current-buffer (htprofile-get-clean-buffer htprofile-statistics-buffer)
    (let (stat-list
          (sort-key-func (intern (concat "htprofile-stat-" (symbol-name htprofile-sort-by)))))
      (dolist (data-list-with-key (htprofile-split-data-list-by-keys))
        (let ((key (car data-list-with-key))
              (data-list (cdr data-list-with-key)))
          (push (htprofile-compute-summary key data-list) stat-list)))
      (setq stat-list (cl-sort stat-list '> :key sort-key-func))
      (let ((inhibit-read-only t))
        (insert (htprofile-get-stat-header))
        (dolist (stat stat-list)
          (insert (htprofile-stat-to-str stat)))))))
(defun htprofile-show-statistics ()
  "show data in a buffer *htprofile-stat*"
  (interactive)
  (htprofile-update-statistics)
  (with-current-buffer (get-buffer htprofile-statistics-buffer)
    (goto-char (point-min))
    (display-buffer (current-buffer))))
(defvar htprofile-max-log
  1000
  "The number of data which are shown by `htprofile-show-log'")
(defvar htprofile--current-log-length)
(defvar htprofile-log-buffer "*htprofile-log*")
(defun htprofile-update-log ()
  (with-current-buffer (htprofile-get-clean-buffer htprofile-log-buffer)
    (let ((inhibit-read-only t))
      (insert (htprofile-get-data-header))
      (dolist (data (htprofile-get-data-list (- htprofile--current-log-length htprofile-max-log)
                                             htprofile--current-log-length))
        (insert (format "%s\n" (htprofile-data-to-str data)))))))
(defun htprofile-show-log ()
  "show data in a buffer *htprofile-log*"
  (interactive)
  (setq htprofile--current-log-length (length (htprofile-get-data-list)))
  (htprofile-update-log)
  (with-current-buffer (get-buffer htprofile-log-buffer)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'htprofile)
