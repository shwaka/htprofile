(require 'ert)

(defun htprofile-test-table--data-formatter (row-data)
  (format "%s" row-data))

(ert-deftest col-format ()
  (let ((col-format (htptable-make-col-format :header "hoge"
                                              :width 10
                                              :data-formatter 'htprofile-test-table--data-formatter
                                              :truncation t)))
    (should (equal (htptable-format-header col-format)
                   "hoge      "))
    (should (equal (htptable-format-cell col-format '(hoge fuga piyo))
                   "(hoge fuga"))))
