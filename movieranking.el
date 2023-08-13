;; -*- lexical-binding: t; -*-
;;;
;;; This is almost how Aladdin must've felt when he stumbled into the
;;; cave and found the Diamond in the Rough, the Genie of the Lamp.
;;;

(require 'parse-csv)

;; I don't really like the syntax of MAPHASH, since it 1. forces you
;; to separate away the table you're operating on, which can be hard
;; to read for nested invocations; 2. otherwise compels you to let-out
;; the lambda you're using, forcing you to either give it a name, or
;; else potentially change what variables it's scoping over (that last
;; point is more of a suspicion, but still.)
;;
;; Also, this macro lets you optionally return a result, which in many
;; cases is the hash-table itself, a feature missing in MAPHASH.
(cl-defmacro dohash ((key value hash-table &optional result) &rest body)
  "Loop over a hash table's key-value pairs, as with MAPHASH.

Evaluate BODY with KEY and VALUE bound, in each iteration, to a
key-value in HASH-TABLE.  Then evaluate RESULT to get the return
value (default NIL).

\(fn (KEY VALUE HASH-TABLE [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp symbolp form &optional form) body)))
  `(progn
     (maphash (lambda (,key ,value) ,@body) ,hash-table)
     ,result))

(defun compute-rater-table (filename)
  "Generate a hash table that maps a rater IDs to another hash
table that maps a movie ID to a rating.

Movies are indicated by unique ID numbers."
  (let ((lists (with-temp-buffer
                 (insert-file-contents filename)
                 (parse-csv-string-rows (buffer-string) ?\, ?\" "\n")))
        (table (make-hash-table :test #'equal)))

    ;; Remove empty keys.
    ;;
    ;; I suspect this happens because of a trailing newline at the end
    ;; of the file, but I'm not entirely sure, so let's assume they
    ;; can occur anywhere before then.
    ;;
    ;; Anyway, this looks like something peculiar to the CSV parser
    ;; we're using; our Java code need not worry about this.
    (cl-delete-if (lambda (rater-id) (string-empty-p rater-id)) (cdr lists) :key #'car)

    (dolist (row (cdr lists) table)     ; skip the header row
      (seq-let (rater-id movie-id rating) row
        (let ((entry (gethash rater-id
                              table
                              (make-hash-table :test #'equal))))
          (puthash movie-id rating entry)
          (puthash rater-id entry table))))))

(defun compute-dot-product (rater-table rater-id-1 rater-id-2)
  "Compute the dot product of two rows in the given RATER-TABLE."
  (let ((row1 (gethash rater-id-1 rater-table))
        (row2 (gethash rater-id-2 rater-table))
        (dot-product 0))
    (dohash (movie-id rating-1 row1 dot-product)
      (cl-flet ((normalize (rating)
                  (- (string-to-number rating) 5)))
        (when-let ((rating-2 (gethash movie-id row2)))
          (cl-incf dot-product
                   (* (normalize rating-1)
                      (normalize rating-2))))))))

(defun compute-full-coefficient-table (rater-table main-rater-id)
  "Generate a hash table that maps a rater ID to the dot product of that rater
and some given \"main\" rater (in practice, the user of the
application.)"
  (let ((coefficient-table (make-hash-table :test #'equal))
        (rater-table-without-main (copy-hash-table rater-table)))

    ;; Don't compute a "dot-square".
    (remhash main-rater-id rater-table-without-main)

    (dohash (rater-id movie-table rater-table-without-main coefficient-table)
      (let ((dot-product (compute-dot-product rater-table main-rater-id rater-id)))
        (puthash rater-id dot-product coefficient-table)))))

(defun compute-refined-coefficient-table (coefficient-table num-similar-raters)
  "Given a coefficient table, generate a new one, but only keep the
raters with the highest coefficients. Of these, filter out any
non-positive cofficients as well."
  (let* ((coefficients (hash-table-values coefficient-table))
         (top-coefficients (seq-take (sort coefficients #'>)
                                     num-similar-raters)))
    (let ((refined-coefficient-table (copy-hash-table coefficient-table)))
      (dohash (rater-id coefficient coefficient-table refined-coefficient-table)
        (unless (and (memql coefficient top-coefficients)
                     (> coefficient 0))
          (remhash rater-id refined-coefficient-table))))))

(defun compute-ratings-table (rater-table)
  "Generate a hash table that maps a movie ID to another hash table that
maps a rater ID to a rating."
  (let ((ratings-table (make-hash-table :test #'equal)))
    (dohash (rater-id movie-table rater-table ratings-table)
      (dohash (movie-id rating movie-table)
        (let ((entry (gethash movie-id
                              ratings-table
                              (make-hash-table :test #'equal))))
          (puthash rater-id rating entry)
          (puthash movie-id entry ratings-table))))))

(defun compute-movie-averages-table (ratings-table refined-coefficient-table min-raters)
  "Generate a hash table that maps a movie ID to a weighted-average rating.

Each movie rating is weighted by the given rater's value in
REFINED-COEFFICIENT-TABLE. If the rater isn't a key in that
table, then it's not included as part of the average.

Also, if a given row in RATINGS-TABLE is smaller than MIN-RATERS,
that movie doesn't get included in the final movie-averages
table (that row is skipped.)"
  (let ((movie-averages-table (make-hash-table :test #'equal)))
    (dohash (movie-id ratings-by-rater ratings-table movie-averages-table)
      (let ((sum 0)
            (i 0))
        (dohash (rater-id rating ratings-by-rater)
          (when-let ((coefficient (gethash rater-id refined-coefficient-table)))
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

(cl-defstruct (movie-info
                (:constructor new-movie-info
                              (id title year country genres directors minutes poster-url)))
  "A struct that bundles a given row of data from a CSV file
detailing info about movies."
  id title year country genres directors minutes poster-url)

(defun compute-movie-data-table (filename)
  "A hash table that maps a movie ID to a MOVIE-INFO struct."
  (let ((lists (with-temp-buffer
                 (insert-file-contents filename)
                 (parse-csv-string-rows (buffer-string) ?\, ?\" "\n")))
        (table (make-hash-table :test #'equal)))

    ;; Remove empty keys
    (cl-delete-if (lambda (rater-id) (string-empty-p rater-id)) (cdr lists) :key #'car)

    (dolist (row (cdr lists) table)
      (puthash (car row) (apply #'new-movie-info row) table))))

;; Use a factory for predicates. This eliminates the dependency on the
;; movie data table that client functions would otherwise inherit.
(defun initialize-predicates (movie-data-table)
  (defun make-genre-p (genre)
    (lambda (movie-id)
      (let* ((full-info (gethash movie-id movie-data-table))
             (genres (movie-info-genres full-info)))
        (string-match genre genres)))))

(defun filter-movie-averages-table (movie-averages-table &rest predicate-fns)
  "Return a copy of the given MOVIE-AVERAGES-TABLE, but filtered by
PREDICATE-FN (for example, a genre filter, year, movie length,
etc.)"
  (cl-flet ((check-all (movie-id preds)
              (cl-reduce (lambda (verdict fn) (and verdict (funcall fn movie-id)))
                         preds
                         :initial-value t)))
    (let ((copy (copy-hash-table movie-averages-table)))
      (dohash (movie-id average movie-averages-table copy)
        (unless (check-all movie-id predicate-fns)
          (remhash movie-id copy))))))

(defun pretty-print-top-ranked-movies-by-title (movie-data-table top-ranked-movie-ids)
  (pcase-dolist (`(,movie-id . ,average) top-ranked-movie-ids)
    (let* ((movie-info (gethash movie-id movie-data-table))
           (title (movie-info-title movie-info)))
      (message "%s ---> %f" title average))))

(defun print-hash-table (hash-table)
  "Print a hash table's key-value pairs."
  (dohash (key value hash-table)
    (message "%s %s" key value)))

(defun main (rater-id min-raters num-similar-raters)
  (let ((rater-table (compute-rater-table "data/ratings.csv")))
    (let* ((full-ctable (compute-full-coefficient-table rater-table rater-id))
           (refined-ctable (compute-refined-coefficient-table full-ctable num-similar-raters))
           (ratings-table (compute-ratings-table rater-table))
           (movie-averages-table (compute-movie-averages-table ratings-table refined-ctable min-raters))

           ;; We may not need 'movie-data-table' as a standalone
           ;; variable?
           (movie-data-table (compute-movie-data-table "data/ratedmoviesfull.csv"))
           (mat-filtered (progn (initialize-predicates movie-data-table)
                                (filter-movie-averages-table movie-averages-table (make-genre-p "Action"))))
           (top-ranked-movie-ids (get-top-ranked-movie-ids mat-filtered)))
      (pcase-let ((`(,top-movie-id . ,average) (car top-ranked-movie-ids)))
        (movie-info-title (gethash top-movie-id movie-data-table))))))

;;; Tests

(require 'ert)

(ert-deftest top-action-movie-is-rush ()
  (should (equal
           "Rush"
           (main "65" 5 20))))
