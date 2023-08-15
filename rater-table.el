;; -*- lexical-binding: t; -*-

(let ((compute-rater-table
       (lambda (filename)
         (let ((csv-data (get-raw-csv-data filename))
               (table (make-hash-table :test #'equal)))
           (pcase-dolist (`(,rater-id ,movie-id ,rating) csv-data)
             (let ((entry (gethash rater-id
                                   table
                                   (make-hash-table :test #'equal))))
               (puthash movie-id rating entry)
               (puthash rater-id entry table)))
           table))))
  (defvar *rater-table* (funcall compute-rater-table "data/ratings.csv")
    "A hash table that maps a rater ID to another hash table that
maps a movie ID to a rating.

Movies are indicated by unique ID numbers."))

(provide 'rater-table)
