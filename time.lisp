
(in-package :gute)

;;; Returns the original output in a list and the elapsed time of running the
;;; function in subsequent values.
;;;
;;; Optional parameter  `unit` can be
;;;   :real - for clock time
;;;   :base - for implementation-specific internal time
(declaim (ftype (function (function &optional keyword)
                          (values list rational &optional))
                timing))
(defun timing (function &optional (unit :real))
  (let* ((time-fn (case unit
                    (:real #'get-internal-real-time)
                    (:base #'get-internal-run-time)
                    (otherwise (error 
                                 "Invalid parameter for `unit` in the `timing` function. It must be :real or :base. Given: ~a."
                                 unit))))
         (start (funcall time-fn))
        result)
    (setf result (multiple-value-list (funcall function))) 
    (values result
            (/ (- (funcall time-fn) start) internal-time-units-per-second))))

