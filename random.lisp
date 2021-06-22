
(in-package :gute)

(defun set-seed (seed)
  "Set randomness seed with a number for replicability, using SBCL
  functionality. This functionality doesn't exist for general CL."
  (setf *random-state* (sb-ext:seed-random-state seed)))

(defun uniform-sample (lst n)
  "Sample uniformly from a list WITH replacement"
  (loop for i from 1 to n
        collect (nth (random (length lst)) lst)))

