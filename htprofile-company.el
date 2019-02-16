;;; htprofile-company.el --- profiler for company    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  shun

;; Author: Shun Wakatsuki <shun.wakatsuki@gmail.com>
;; Keywords: convenience
;; Package-Requires: ()

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

(require 'htprofile-widgets)

(defun htprofile-advice:company-call-backend (orig-func &rest args)
  (let ((start-time (current-time))
        end-time
        elapsed-time)
    (apply orig-func args)
    (setq end-time (current-time)
          elapsed-time (time-subtract end-time start-time))
    (my-message "%S" (list args (float-time elapsed-time)))))
(advice-add 'company-call-backend :around 'htprofile-advice:company-call-backend)

(provide 'htprofile-company)
;;; htprofile-company.el ends here
