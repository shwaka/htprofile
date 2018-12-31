;;; htprofile-widgets.el --- Widgets used in htprofile.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shun Wakatsuki

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
;; Widgets used in htprofile.el.

;;; Code:

(require 'cl-lib)

;;; textfield
(cl-defstruct (htpwidget-textfield (:constructor make-htpwidget-textfield--internal))
  text name)

(cl-defun make-htpwidget-textfield (&key text name)
  (let (name-symbol)
    (cond
     ((null name)
      (setq name-symbol (intern (secure-hash 'md5 (format "%s:%s"
                                            (current-time)
                                            text)))))
     ((symbolp name)
      (setq name-symbol name))
     ((stringp name)
      (setq name-symbol (intern name)))
     (t
      (error "invalid name in make-htpwidget-textfield")))
    (make-htpwidget-textfield--internal :text text :name name-symbol)))

(defun htpwidget-insert-textfield (textfield)
  (let* ((inhibit-read-only t)
         (text-raw (htpwidget-textfield-text textfield))
         (text (if (eq 0 (length text-raw))
                   "?"
                 text-raw))
         (name (htpwidget-textfield-name textfield)))
    (insert (propertize text
                        'htpwidget-textfield name))))

(defun htpwidget-update-textfield (textfield text)
  "Update TEXTFIELD with TEXT.

Point will move to the end of the updated text."
  (setf (htpwidget-textfield-text textfield) text)
  (let* ((inhibit-read-only t)
         (name (htpwidget-textfield-name textfield))
         (begin (text-property-any (point-min) (point-max)
                                   'htpwidget-textfield name))
         (end (if begin
                  (or (next-single-property-change begin 'htpwidget-textfield)
                      (point-max))
                nil)))
    (when (and begin end)
      (goto-char begin)
      (delete-char (- end begin))
      (htpwidget-insert-textfield textfield))))

;;; variable
(defface htpwidget-variable-face
  '((t
     :inherit font-lock-constant-face
     ))
  "face for variable value")

(cl-defstruct (htpwidget-variable (:constructor make-htpwidget-variable--internal))
  symbol type textfield candidates after-update-hook)

(cl-defun make-htpwidget-variable (&key symbol type candidates after-update-hook)
  (unless (find type '(integer string))
    (error "invalid variable type"))
  (let* ((text (format "%s" (eval symbol)))
         (name (intern (format "htpwidget-variable:%s" (symbol-name symbol))))
         (tf (make-htpwidget-textfield :text (propertize text
                                                         'face 'htpwidget-variable-face)
                                       :name name)))
    (make-htpwidget-variable--internal :symbol symbol
                                      :type type
                                      :textfield tf
                                      :candidates candidates
                                      :after-update-hook after-update-hook)))

(defun htpwidget-variable-value (variable)
  (eval (htpwidget-variable-symbol variable)))

(defun htpwidget-update-variable (variable value)
  (let* ((symbol (htpwidget-variable-symbol variable))
         (tf (htpwidget-variable-textfield variable))
         (old-value (eval symbol))
         (after-update-hook (htpwidget-variable-after-update-hook variable)))
    (set symbol value)
    (htpwidget-update-textfield tf (propertize (format "%s" (eval symbol))
                                               'face 'htpwidget-variable-face))
    (when (and (not (equal value old-value))
               (functionp after-update-hook))
      (funcall after-update-hook))))

(defun htpwidget-insert-variable-value (variable)
  (htpwidget-insert-textfield (htpwidget-variable-textfield variable)))

(defun htpwidget-edit-variable (variable)
  (let* ((old-value (htpwidget-variable-value variable))
         (symbol (htpwidget-variable-symbol variable))
         (type (htpwidget-variable-type variable))
         (candidates (htpwidget-variable-candidates variable))
         prompt
         new-value)
    (cond
     ((eq type 'integer)
      (setq prompt (format "Input %s (integer, default %s): "
                           symbol
                           old-value))
      (setq new-value
            (read-from-minibuffer prompt
                                  nil nil t nil
                                  (format "%s" old-value)))
      (unless (integerp new-value)
        (error "Please input an integer")))
     ((eq type 'string)
      (setq prompt (format "Input %s (string, default %s): "
                           symbol
                           old-value))
      (setq new-value
            (completing-read prompt candidates nil t
                             nil nil old-value))))
    (htpwidget-update-variable variable new-value)))

;;; button to edit variables
;;; EVbutton = Edit Variable button
(define-button-type 'htpwidget-evbutton
  'action 'htpwidget-evbutton-pressed
  'follow-link t)

(defun htpwidget-evbutton-pressed (button)
  (let ((variable-list (button-get button 'htpwidget-variable-list)))
    (dolist (var variable-list)
      (save-excursion
        (htpwidget-edit-variable var)))))

(defun htpwidget-insert-evbutton (text variable-list)
  (insert-button text :type 'htpwidget-evbutton
                 'htpwidget-variable-list variable-list))

(provide 'htprofile-widgets)
;;; htprofile-widgets.el ends here
