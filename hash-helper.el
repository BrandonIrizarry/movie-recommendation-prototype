;; -*- lexical-binding: t; -*-
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

(defun hash-table-delete-if (hash-table predicate)
  "Destructively remove all key-value pairs of HASH-TABLE whenever
PREDICATE returns T.

PREDICATE is a function that accepts the key and value as
arguments, in that order."
  (let ((copy (copy-hash-table hash-table)))
    (dohash (key value copy hash-table)
      (when (funcall predicate key value)
        (remhash key hash-table)))))

(defun hash-table-keep-if (hash-table predicate)
  "Keep only those key-value pairs for which PREDICATE returns T;
destructively remove the rest.

PREDICATE is a function that accepts the key and value as
arguments, in that order."
  (cl-flet ((negate (fn)
              (lambda (&rest _)
                (not (apply fn _)))))
    (hash-table-delete-if hash-table (negate predicate))))

(provide 'hash-helper)
