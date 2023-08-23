;; -*- lexical-binding: t; -*-

(defun plist-keys (plist)
  "Return the list of keys of PLIST."
  (let ((grouping (seq-partition plist 2)))
    (mapcar #'car grouping)))

(provide 'util)
