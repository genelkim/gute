
(in-package :util)

;;; Based on Rosetta Code: https://www.rosettacode.org/wiki/Time_a_function#Common_Lisp
;;; Returns the original output in a list and the elapsed time of running the
;;; function in subsequent values.
;;;
;;; Optional parameter  `unit` can be
;;;   :real - for clock time
;;;   :base - for implementation-specific internal time
(defun timing (function &optional (unit ':real))
  (let* ((time-fn (case unit
                    (':real #'get-internal-real-time)
                    (':base #'get-internal-base-time)
                    (otherwise (error 
                                 "Invalid parameter for `unit` in the `timing` function. It must be :real or :base. Given: ~a."
                                 unit))))
         (start (funcall time-fn))
        result)
    (setf result (multiple-value-list (funcall function))) 
    (values result
            (/ (- (funcall time-fn) start) internal-time-units-per-second))))

