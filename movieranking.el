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
  "Generate a hash table mapping rater IDs to information about
movies rated.

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

(defun compute-ratings-table (rater-table)
  (let ((ratings-table (make-hash-table :test #'equal)))
    (dohash (rater-id movie-table rater-table ratings-table)
      (dohash (movie-id rating movie-table)
        (let ((entry (gethash movie-id
                              ratings-table
                              (make-hash-table :test #'equal))))
          (puthash rater-id rating entry)
          (puthash movie-id entry ratings-table))))))

(defun compute-coefficient-table (rater-table rater-id)
  (error "To be implemented."))

(defun print-hash-table (hash-table)
  "Print a hash table's key-value pairs."
  (dohash (key value hash-table)
    (message "%s %s" key value)))

(let ((table (compute-rater-table "data/ratings_short.csv")))
  (print-hash-table table)
  (compute-dot-product table "1" "2")
  (let ((result (compute-ratings-table table)))
    (print-hash-table result)))
