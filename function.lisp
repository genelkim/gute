
(in-package :util)


;; Function that just returns nil.
(defun nilfn (x)
  (declare (ignore x))
  nil)


;; Function that just returns t.
(defun tfn (x)
  (declare (ignore x))
  t)


;; Composes any number of one argument functions.
;; Modified from https://rosettacode.org/wiki/Function_composition#Common_Lisp
(defun compose (&rest args) 
	(lambda (x)
    (reduce #'(lambda (acc fn)
                (funcall fn acc))
            (reverse args) :initial-value x)))

