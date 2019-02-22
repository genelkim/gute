
(in-package :util)

;; Composes any number of one argument functions.
;; Modified from https://rosettacode.org/wiki/Function_composition#Common_Lisp
(defun compose (&rest args) 
	(lambda (x)
    (reduce #'(lambda (acc fn)
                (funcall fn acc))
            (reverse args) :initial-value x)))

