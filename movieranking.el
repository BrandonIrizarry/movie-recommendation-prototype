;; -*- lexical-binding: t; -*-

(require 'parse-csv)

(defun parse-csv-file (filename)
  (let ((lists (with-temp-buffer
                 (insert-file filename)
                 (parse-csv-string-rows (buffer-string) ?\, ?\" "\n")))
        (table (make-hash-table :test #'equal)))
    ;; Skip the header row
    (dolist (row (cdr lists) table)
      (let* ((rater-id (car row))
             (movie-id (cadr row))
             (entry (gethash rater-id
                             table
                             (make-hash-table :test #'equal))))
        (puthash movie-id (cddr row) entry)
        (puthash rater-id entry table)))))
