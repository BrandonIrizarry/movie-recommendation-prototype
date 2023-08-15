;; -*- lexical-binding: t; -*-

(require 'parse-csv)

(defun get-raw-csv-data (filename)
  "Parse a CSV file using the library PARSE-CSV, removing any empty
keys.

Return the list of rows that PARSE-CSV-STRING-ROWS produces."
  (let ((csv-data
         (with-temp-buffer
           (insert-file-contents filename)
           (parse-csv-string-rows (buffer-string) ?\, ?\" "\n"))))
    (cl-remove-if #'string-empty-p
                  (cdr csv-data) ; skip header row.
                  :key #'car)))

(provide 'csv-helper)
