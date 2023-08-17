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

(cl-defun hash-table-delete-if (hash-table predicate &key by)
  "Destructively remove all key-value pairs of HASH-TABLE whenever
PREDICATE returns T.

PREDICATE is a function that accepts the key and value as
arguments, in that order."
  (when (null by)
    (error "Missing keyword"))
  (let ((keywords '(key value both)))
    (unless (memq by keywords)
      (error "Invalid keyword %s" by)))
  (let ((copy (copy-hash-table hash-table)))
    (dohash (key value copy hash-table)
      (let ((test (pcase by
                    ('key (funcall predicate key))
                    ('value (funcall predicate value))
                    ('both (funcall predicate key value)))))
        (when test
          (remhash key hash-table))))))

(cl-defun hash-table-keep-if (hash-table predicate &key by)
  "Keep only those key-value pairs for which PREDICATE returns T;
destructively remove the rest.

PREDICATE is a function that accepts the key and value as
arguments, in that order."
  (cl-flet ((negate (fn)
              (lambda (&rest _)
                (not (apply fn _)))))
    (hash-table-delete-if hash-table (negate predicate) :by by)))

(provide 'hash-helper)
