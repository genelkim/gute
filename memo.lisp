
(in-package :gute)

;; Memoization facility from Peter Norvig's PIOP
;; http://norvig.com/paip/auxfns.lisp
;; Defaults have been changed for more flexibility.

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defvar *global-memo-lock* (bt:make-lock))
(defun memo (fn &key (key #'identity) (test #'equal) name)
  "Return a memo-function of fn."
  (declare (type function fn key test)
           (type symbol name))
  (let ((table (make-hash-table :test test))
        (tablelock (bt:make-lock)))
    (bt:with-lock-held (*global-memo-lock*)
      (setf (get name 'memo) table))
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p) (gethash k table)
            (values-list
              (if found-p val
                  (let ((result (multiple-value-list (apply fn args))))
                    (bt:with-lock-held (tablelock)
                      (setf (gethash k table) result))))))))))

;; NB: This doesn't work on self-recursive functions.
;; Use (setf (fdefinition fn-name) (memo #'fn)) for those cases.
(defun memoize (fn-name &key (key #'identity) (test #'equal))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

