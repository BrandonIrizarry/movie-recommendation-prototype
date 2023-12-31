;; -*- lexical-binding: t; -*-
;;;
;;; This is almost how Aladdin must've felt when he stumbled into the
;;; cave and found the Diamond in the Rough, the Genie of the Lamp.
;;;

(let ((load-path (cons (expand-file-name ".") load-path)))
  (require 'csv-helper)
  (require 'hash-helper)
  (require 'rater-table)
  (require 'movie-data-table)
  (require 'util))

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

    ;; Compute the initial coefficient table.
    (dohash (rater-id _ rater-table-without-main)
      (let ((dot-product (compute-dot-product main-rater-id rater-id)))
        (puthash rater-id dot-product coefficient-table)))

    ;; Filter out non-positive and low-ranking coefficients,
    (let* ((coefficients (hash-table-values coefficient-table))
           (top-coefficients (seq-take (sort coefficients #'>) num-similar-raters))
           (bad-coefficient-p (lambda (coefficient)
                                (or (<= coefficient 0)
                                    (not (memql coefficient top-coefficients))))))
      (hash-table-delete-if coefficient-table bad-coefficient-p :by 'value))))

(defun compute-ratings-table (&rest filters)
  "Generate a hash table that maps a movie ID to another hash table that
maps a rater ID to a rating.

Filter according to keyword args FILTERS."

  ;; Construct the initial table.
  (let ((ratings-table (make-hash-table :test #'equal)))
    (dohash (rater-id movie-table *rater-table* ratings-table)
      (dohash (movie-id rating movie-table)
        (let ((entry (gethash movie-id
                              ratings-table
                              (make-hash-table :test #'equal))))
          (puthash rater-id rating entry)
          (puthash movie-id entry ratings-table))))

    ;; Tell program how to define a predicate, given a keyword and
    ;; corresponding argument.
    ;;
    ;; Each lambda body takes advantange of the lexical availability
    ;; of FILTERS.
    (let ((predicate-table
           `(:genre
             ,(lambda (movie-id)
                (let* ((full-info (gethash movie-id *movie-data-table*))
                       (genres (movie-info-genres full-info)))
                  (string-match (plist-get filters :genre) genres)))

             :directors
             ,(lambda (movie-id)
                (let* ((full-info (gethash movie-id *movie-data-table*))
                       (directors (movie-info-directors full-info)))
                  (seq-intersection (split-string directors "," t)
                                    (split-string (plist-get filters :directors) "," t))))

             :minutes
             ,(lambda (movie-id)
                (seq-let (min max) (plist-get filters :minutes)
                  (let* ((full-info (gethash movie-id *movie-data-table*))
                         (duration (movie-info-minutes full-info)))
                    (<= min (string-to-number duration) max))))

             :year
             ,(lambda (movie-id)
                (let* ((full-info (gethash movie-id *movie-data-table*))
                       (year (movie-info-year full-info)))
                  (<= (plist-get filters :year) (string-to-number year)))))))

      ;; Filter out movies according to the filters defined in
      ;; FILTERS.
      (dolist (filter (plist-keys filters) ratings-table)
        (hash-table-keep-if ratings-table (plist-get predicate-table filter) :by 'key)))))

(defun compute-movie-averages-table (ratings-table coefficient-table min-raters)
  "Generate a hash table that maps a movie ID to a weighted-average rating.

Each movie rating is weighted by the given rater's value in
COEFFICIENT-TABLE. Any rater not in COEFFICIENT-TABLE is skipped.

Any row in RATINGS-TABLE smaller than MIN-RATERS is skipped."
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
movies, along with their weighted averages.

With optional argument TOP-N, only return the top N ranked
movies."
  (cl-flet ((better-movie-p (lambda (pair1 pair2)
                              (> (cdr pair1)
                                 (cdr pair2)))))
    (let (pairs)
      (dohash (movie-id average movie-averages-table)
        (push (cons movie-id average) pairs))
      (let ((sorted (sort pairs #'better-movie-p)))
        (if top-n
            (seq-take sorted top-n)
          sorted)))))

(defun main (rater-id min-raters num-similar-raters &rest filters)
  (let* ((ctable (compute-coefficient-table rater-id num-similar-raters))
         (ratings-table (apply #'compute-ratings-table filters))
         (movie-averages-table (compute-movie-averages-table ratings-table ctable min-raters))
         (top-ranked-movie-ids (get-top-ranked-movie-ids movie-averages-table)))
    (pcase-let ((`(,top-movie-id . ,average) (car top-ranked-movie-ids)))
      (movie-info-title (gethash top-movie-id *movie-data-table*)))))

;;; Tests.

(require 'ert)

(ert-deftest top-action-movie-is-rush ()
  (should (equal
           "Rush"
           (main "65" 5 20 :genre "Action"))))

(ert-deftest top-movie-is-the-fault-in-our-stars ()
  (should (equal
           "The Fault in Our Stars"
           (main "65" 5 20))))

(ert-deftest top-csdo-is-unforgiven ()
  (let ((directors "Clint Eastwood,Sydney Pollack,David Cronenberg,Oliver Stone"))
    (should (equal
             "Unforgiven"
             (main "1034" 3 10 :directors directors)))))

(ert-deftest top-adventure-100-200-is-interstellar ()
  (should (equal
           "Interstellar"
           (main "65" 5 10 :genre "Adventure" :minutes '(100 200)))))

(ert-deftest top-year-2000-minutes-80-100-is-the-grand-budapest-hotel ()
  (should (equal
           "The Grand Budapest Hotel"
           (main "65" 5 10 :year 2000 :minutes '(80 100)))))
