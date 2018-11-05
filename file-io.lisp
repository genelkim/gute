;; Reads a file line by line and return a list of strings.
(defun read-file-lines (filename)
  (labels
    ((helper
       (in acc)
       (multiple-value-bind
         (line end?)
         (read-line in nil)
         (if end? 
           (cons line acc)
           (helper in (cons line acc))))))
    (reverse (remove-if #'null (helper (open filename) '())))))

;; Read a file by lisp objects.
;; Returns a list of lisp objects.
(defun read-file-objects (filename)
  (labels
    ((helper
       (in acc)
       (let ((entry (read in nil)))
         (if (null entry)
           acc
           (helper in (cons entry acc))))))
    (reverse (helper (open filename) '())))) 

