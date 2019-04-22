
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


;; Returns an alist of the function argument list grouped by argument type.
;;  e.g. (grouped-arglist #'reduce)
;;       -> ((&required . (function sequence))
;;           (&key . (key from-end start end initial-value))
;;           (&optional . nil)
;;           (&rest . nil))
;;       (grouped-arglist #'mapcar)
;;       -> ((&required . (function list))
;;           (&key . nil)
;;           (&optional . nil)
;;           (&rest . (more-lists)))
(defun grouped-arglist (fn)
  (let* ((arglst (conium:arglist fn))
         (curtype '&required)
         (argtypes '(&required &key &optional &rest))
         (all-groups (pairlis argtypes '(nil nil nil nil)))
         curlst)
    (loop
      for arg in arglst
      do (if (member arg argtypes)
           (progn
             (rplacd (assoc curtype all-groups) curlst)
             (setf curtype arg)
             (setf curlst nil))
           (setf curlst
                 ;; Add arg to list, if there's a default value, ignore it.
                 (cons (if (consp arg) (car arg) arg) curlst))))
    (if (not (null curlst))
      (rplacd (assoc curtype all-groups) curlst))
    all-groups))


;; Returns the number of required arguments in the given function.
;; Required arguments don't include &key &optional or &rest arguments.
(defun argnum (fn &optional (argtypes '(&required &key &optional &rest)))
  (let ((grp-arglst (grouped-arglist fn)))
    (apply #'+
           (mapcar #'(lambda (argtype)
                       (length (cdr (assoc argtype grp-arglst))))
                   argtypes))))

(defun required-argnum (fn) (argnum fn '(&required)))
(defun key-argnum (fn) (argnum fn '(&key)))
(defun optional-argnum (fn) (argnum fn '(&optional)))

