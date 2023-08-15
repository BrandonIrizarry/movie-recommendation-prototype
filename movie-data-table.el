;; -*- lexical-binding: t; -*-

(require 'csv-helper)

(cl-defstruct (movie-info
                (:constructor new-movie-info
                              (id title year country genres directors minutes poster-url)))
  "A struct that bundles a given row of data from a CSV file
detailing info about movies."
  id title year country genres directors minutes poster-url)

(let ((compute-movie-data-table
       (lambda (filename)
         (let ((csv-data (get-raw-csv-data filename))
               (table (make-hash-table :test #'equal)))
           (dolist (row csv-data table)
             (puthash (car row) (apply #'new-movie-info row) table))))))
  (defvar *movie-data-table* (funcall compute-movie-data-table "data/ratedmoviesfull.csv")
    "A hash table that maps a movie ID to a MOVIE-INFO struct."))

(provide 'movie-data-table)
