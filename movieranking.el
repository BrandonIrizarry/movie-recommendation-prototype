;; -*- lexical-binding: t; -*-
;;;
;;; This is almost how Aladdin must've felt when he stumbled into the
;;; cave and found the Diamond in the Rough, the Genie of the Lamp.
;;;

(require 'parse-csv)

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
      (let* ((rater-id (car row))
             (movie-id (cadr row))
             (entry (gethash rater-id
                             table
                             (make-hash-table :test #'equal))))
        (puthash movie-id (cddr row) entry)
        (puthash rater-id entry table)))))

(defun compute-dot-product (rater-table rater-id-1 rater-id-2)
  "Compute the dot product of two rows in the given RATER-TABLE."
  (let ((row1 (gethash rater-id-1 rater-table))
        (row2 (gethash rater-id-2 rater-table))
        (dot-product 0))
    (maphash (lambda (movie-id info-1)
               (cl-flet ((normalize (rating)
                           (- (string-to-number rating) 5)))
                 (let ((rating-1 (car info-1)))
                   (when-let ((info-2 (gethash movie-id row2)))
                     (let ((rating-2 (car info-2)))
                       (cl-incf dot-product
                                (* (normalize rating-1)
                                   (normalize rating-2))))))))
             row1)
    dot-product))

(defun compute-movie-table (rater-table)
  (let ((table (make-hash-table :test #'equal)))
    (maphash (lambda (rater-id movie-table)
               (maphash (lambda (movie-id info)
                          (let ((entry (gethash movie-id
                                                table
                                                (make-hash-table :test #'equal))))
                            (puthash rater-id info entry)
                            (puthash movie-id entry table)))
                        movie-table))
             rater-table)
    table))

(defun compute-coefficient-table (rater-table rater-id)
  (error "To be implemented."))

(let ((table (compute-rater-table "data/ratings_short.csv")))
  (compute-dot-product table "1" "2")
  (compute-movie-table table))
