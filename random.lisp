
(in-package :gute)

(defun set-seed (seed)
  "Set randomness seed with a number for replicability, using SBCL
  functionality. This functionality doesn't exist for general CL."
  (setf *random-state* (sb-ext:seed-random-state seed)))

(defun uniform-sample (lst n)
  "Sample uniformly from a list WITH replacement"
  (loop for i from 1 to n
        collect (nth (random (length lst)) lst)))

; TODO: write a more efficient version https://github.com/enewe101/categorical
(defun categorical-sample (cats &optional (n 1))
  "Samples from a categorical distribution.
   
   NB: The results will be sorted in the reverse order of cats.

   Arguments
   ---------
   cats : a association list from categories to weights to sample from
   n : the number of samples desired"
  ;; NB: d0 ending ensures double floats for precision.
  (let ((sample-vals 
          (sort (loop for x from 0 upto n collect (random 1.0d0))
                #'<))
        (curval 0.0d0)
        samples)
    (format t "sample-vals: ~s~%" sample-vals)
    (loop for (c . w) in cats
          do (incf curval w)
          do (loop while (and sample-vals
                              (< (car sample-vals) curval))
                   do (push c samples)
                   do (setf sample-vals (cdr sample-vals))))
    samples))

