(require 'htprofile-table)
(require 'htprofile-viewer)

(defun htprofile-itest-table--data-formatter (row-data)
  (format "%s" row-data))

(defvar htprofile-itest-col-format-1
  (htptable-make-col-format :header "hoge"
                            :width 10
                            :data-formatter 'htprofile-itest-table--data-formatter))

(defvar htprofile-itest-col-format-2
  (htptable-make-col-format :header "fuga"
                            :width 5
                            :data-formatter 'htprofile-itest-table--data-formatter))

(defvar htprofile-itest-col-format-3
  (htptable-make-col-format :header "piyo"
                            :width 5
                            :data-formatter 'htprofile-itest-table--data-formatter
                            :align 'right))

(defvar htprofile-itest-col-format-last
  (htptable-make-col-format :header "last"
                            :width nil
                            :data-formatter 'htprofile-itest-table--data-formatter))

(defvar htprofile-itest-row-data-list
  (list '(abc def ghi) 'fooo 'aiueo
        "aiueokakikukekosasisuseso"))

(defvar htprofile-itest-table
  (htptable-make-table :col-format-list (list htprofile-itest-col-format-1
                                              htprofile-itest-col-format-2
                                              htprofile-itest-col-format-3
                                              htprofile-itest-col-format-last)
                       :row-data-list htprofile-itest-row-data-list))

(defun htprofile-itest-show-table ()
  (interactive)
  (let ((viewer (htpviewer-make-viewer :buffer-name "*hoge*"
                                       :table htprofile-itest-table)))
    (htpviewer-show-viewer viewer))
  ;; (insert (htptable-table-to-string htprofile-itest-table))
  )
