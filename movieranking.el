;; -*- lexical-binding: t; -*-
;;;
;;; This is almost how Aladdin must've felt when he stumbled into the
;;; cave and found the Diamond in the Rough, the Genie of the Lamp.
;;;

(let ((load-path (cons (expand-file-name ".") load-path)))
  (require 'csv-helper)
  (require 'hash-helper)
  (require 'rater-table)
  (require 'movie-data-table))

(defun compute-dot-product (rater-id-1 rater-id-2)
  "Compute the dot product of two rows in the given RATER-TABLE."
  (let ((row1 (gethash rater-id-1 *rater-table*))
        (row2 (gethash rater-id-2 *rater-table*))
        (dot-product 0))
    (dohash (movie-id rating-1 row1 dot-product)
      (cl-flet ((normalize (rating)
                  (- (string-to-number rating) 5)))
        (when-let ((rating-2 (gethash movie-id row2)))
          (cl-incf dot-product
                   (* (normalize rating-1)
                      (normalize rating-2))))))))

(defun compute-coefficient-table (main-rater-id num-similar-raters)
  "Generate a hash table that maps a rater ID to the dot product of
that rater and some given \"main\" rater (in practice, the user
of the application.) This dot product is called a
\"coefficient.\"

Only keep raters with positive and high ranking (among top
NUM-SIMILAR-RATERS) coefficients."
  (let ((coefficient-table (make-hash-table :test #'equal))
        (rater-table-without-main (copy-hash-table *rater-table*)))

    ;; Avoid comparison with oneself.
    (remhash main-rater-id rater-table-without-main)

    ;; Compute the initial coefficient table
    (dohash (rater-id movie-table rater-table-without-main)
      (let ((dot-product (compute-dot-product main-rater-id rater-id)))
        (puthash rater-id dot-product coefficient-table)))

    ;; Filter out non-positive and low-ranking coefficients
    (let* ((coefficients (hash-table-values coefficient-table))
           (top-coefficients (seq-take (sort coefficients #'>) num-similar-raters)))
      (hash-table-delete-if coefficient-table (lambda (rater-id coefficient)
                                                (or (<= coefficient 0)
                                                    (not (memql coefficient top-coefficients))))))))

(defun compute-ratings-table ()
  "Generate a hash table that maps a movie ID to another hash table that
maps a rater ID to a rating."
  (let ((ratings-table (make-hash-table :test #'equal)))
    (dohash (rater-id movie-table *rater-table* ratings-table)
      (dohash (movie-id rating movie-table)
        (let ((entry (gethash movie-id
                              ratings-table
                              (make-hash-table :test #'equal))))
          (puthash rater-id rating entry)
          (puthash movie-id entry ratings-table))))))

(defun compute-movie-averages-table (ratings-table coefficient-table min-raters)
  "Generate a hash table that maps a movie ID to a weighted-average rating.

Each movie rating is weighted by the given rater's value in
COEFFICIENT-TABLE. If the rater isn't a key in that
table, then it's not included as part of the average.

Also, if a given row in RATINGS-TABLE is smaller than MIN-RATERS,
that movie doesn't get included in the final movie-averages
table (that row is skipped.)"
  (let ((movie-averages-table (make-hash-table :test #'equal)))
    (dohash (movie-id ratings-by-rater ratings-table movie-averages-table)
      (let ((sum 0)
            (i 0))
        (dohash (rater-id rating ratings-by-rater)
          (when-let ((coefficient (gethash rater-id coefficient-table)))
            (cl-incf sum (* coefficient
                            (string-to-number rating)))
            (cl-incf i)))
        (when (>= i min-raters)
          (puthash movie-id (/ sum (float i)) movie-averages-table))))))

(defun get-top-ranked-movie-ids (movie-averages-table &optional top-n)
  "Using MOVIE-AVERAGES-TABLE, generate a descending-sorted list of
movies, along with their weighted averages."
  (cl-flet ((better-movie-p (lambda (pair1 pair2)
                              (> (cdr pair1)
                                 (cdr pair2)))))
    (let (pairs)
      (dohash (movie-id average movie-averages-table)
        (push (cons movie-id average) pairs))
      (let ((sorted (sort pairs #'better-movie-p)))
        (seq-take sorted (or top-n
                             (length sorted)))))))

;; Use a factory for predicates. This eliminates the dependency on the
;; movie data table that client functions would otherwise inherit.
(defun initialize-predicates ()
  (defun make-genre-p (genre)
    (lambda (movie-id)
      (let* ((full-info (gethash movie-id *movie-data-table*))
             (genres (movie-info-genres full-info)))
        (string-match genre genres)))))

(defun filter-movie-averages-table (movie-averages-table predicate-fn)
  "Return a copy of the given MOVIE-AVERAGES-TABLE, but filtered by
PREDICATE-FN (for example, a genre filter, year, movie length,
etc.)"
  (let ((copy (copy-hash-table movie-averages-table)))
    (dohash (movie-id average movie-averages-table copy)
      (unless (funcall predicate-fn movie-id)
        (remhash movie-id copy)))))

(defun main (rater-id min-raters num-similar-raters &rest filters)
  (cl-flet ((yes (movie-id) t))
    (let ((filters (or filters (cons #'yes filters))))
      (let* ((ctable (compute-coefficient-table rater-id num-similar-raters))
             (ratings-table (compute-ratings-table))
             (movie-averages-table (compute-movie-averages-table ratings-table ctable min-raters))
             (mat-filtered (progn (initialize-predicates)
                                  (filter-movie-averages-table movie-averages-table
                                                               (if-let ((genre (plist-get filters :genre)))
                                                                   (make-genre-p genre)
                                                                   #'yes))))
               (top-ranked-movie-ids (get-top-ranked-movie-ids mat-filtered)))
          (pcase-let ((`(,top-movie-id . ,average) (car top-ranked-movie-ids)))
            (movie-info-title (gethash top-movie-id *movie-data-table*)))))))

;;; Tests

(require 'ert)

(ert-deftest top-action-movie-is-rush ()
  (should (equal
           "Rush"
           (main "65" 5 20 :genre "Action"))))

(ert-deftest top-movie-is-the-fault-in-our-stars ()
  (should (equal
           "The Fault in Our Stars"
           (main "65" 5 20))))
