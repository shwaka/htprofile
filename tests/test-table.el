(require 'ert)

(defun htprofile-test-table--data-formatter (row-data)
  (format "%s" row-data))

(ert-deftest col-format ()
  (let ((col-format (htptable-make-col-format
                     :header "hoge"
                     :width 10
                     :data-formatter 'htprofile-test-table--data-formatter)))
    (should (equal (htptable-format-header col-format)
                   "hoge      "))
    (should (equal (htptable-format-cell col-format '(hoge fuga piyo))
                   "(hoge fuga"))))

(ert-deftest table ()
  (let* ((col-format1 (htptable-make-col-format
                       :header "hoge"
                       :width 10
                       :data-formatter 'htprofile-test-table--data-formatter))
         (col-format2 (htptable-make-col-format
                       :header "fuga"
                       :width 5
                       :data-formatter 'htprofile-test-table--data-formatter))
         (row-data-list (list '(abc def ghi) 'fooooo "aiueokakiku"))
         (table (htptable-make-table :col-format-list (list col-format1 col-format2)
                                     :row-data-list row-data-list)))
    (htptable-table-to-string table)))
