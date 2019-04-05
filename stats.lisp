
(in-package :util)

;; Input data format:
;;   List of <boolean>
;; Returns the precision (<number true>/<total number>).
(defun precision (data &key (make-float t))
  (declare (type list data))
  (let ((rat
          (/ (length (remove-if-not #'(lambda (x) x) data)) (length data))))
    (if make-float (float rat) rat)))


;; Input data format:
;;   List of (category . <boolean>)
;; Returns:
;;   List of (category . precision)
(defun group-precisions (data)
  (let (grouped)
    (setq grouped
          ;; Accumulate the association list by the key.
          (reduce #'(lambda (acc new)
                      (let ((cat (car new))
                            (val (cdr new)))
                        (if (not (assoc cat acc :test #'equal))
                          (setq acc (acons cat nil acc)))
                        (rplacd (assoc cat acc :test #'equal)
                                (cons val (cdr (assoc cat acc :test #'equal))))
                        acc))
                      data :initial-value nil))
    (mapcar #'(lambda (x)
                (cons (car x)
                      (precision (cdr x))))
            grouped)))

;; Input data format:
;;   (category . <boolean>)
;; Returns the macro precision -- precision over the full set.
(defun macro-precision (data)
  (precision (mapcar #'cdr data)))

;; Input data format:
;;   List of (category . <boolean>)
;; Returns the micro precision -- average precision over the categories.
(defun micro-precision (data)
  (cl-mathstats:mean (mapcar #'cdr (group-precisions data))))

