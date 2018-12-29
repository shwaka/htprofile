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
                        'face 'font-lock-keyword-face
                        'htpwidget-textfield name))))

(defun htpwidget-update-textfield (textfield text)
  (save-excursion
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
      (htpwidget-insert-textfield textfield)))))

;;; variable
(cl-defstruct (htpwidget-variable (:constructor make-htpwidget-variable--internal))
  symbol type textfield candidates)

(cl-defun make-htpwidget-variable (&key symbol type candidates)
  (unless (find type '(integer string))
    (error "invalid variable type"))
  (let* ((text (format "%s" (eval symbol)))
         (name (intern (format "htpwidget-variable:%s" (symbol-name symbol))))
         (tf (make-htpwidget-textfield :text text :name name)))
    (make-htpwidget-variable--internal :symbol symbol
                                      :type type
                                      :textfield tf
                                      :candidates candidates)))

(defun htpwidget-variable-value (variable)
  (eval (htpwidget-variable-symbol variable)))

(defun htpwidget-update-variable (variable value)
  (let ((symbol (htpwidget-variable-symbol variable))
        (tf (htpwidget-variable-textfield variable)))
    (set symbol value)
    (htpwidget-update-textfield tf (format "%s" (eval symbol)))))

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
  'action 'htpwidget-evbutton-pressed)

(defun htpwidget-evbutton-pressed (button)
  (let ((variable-list (button-get button 'htpwidget-variable-list)))
    (dolist (var variable-list)
      (htpwidget-edit-variable var))))

(defun htpwidget-insert-evbutton (text variable-list)
  (let ((beg (point))
        end)
    (insert text)
    (setq end (point))
    (make-button beg end
                 :type 'htpwidget-evbutton
                 'htpwidget-variable-list variable-list)))


(provide 'htprofile-widget)
