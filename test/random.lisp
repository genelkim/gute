
(in-package :gute/tests)

(define-test set-seed
  "Test that set-seed actually makes things replicable."
  (:tag :random)
  (let (a1 a2 b1 b2 a3 r1 r2)
    (set-seed 1)
    (setf a1 (random 1000))
    (setf r1 (random 1000))
    (assert-false (equal a1 r1))

    ;; reset seed
    (set-seed 1)
    (setf a2 (random 1000))
    (assert-equal a1 a2)
    (setf r2 (random 1000))
    (assert-equal r1 r2)
    (assert-false (equal a2 r2))

    ;; set seed to different value
    (set-seed 2)
    (setf b1 (random 1000))
    (set-seed 2)
    (setf b2 (random 1000))
    (assert-equal b1 b2)
    (assert-false (equal a1 b1))))

(define-test uniform-sample
  "Test basic features of uniform-sample."
  (:tag :random)
  (let ((population (loop for i from 1 to 1000 collect i)))
    ;; We get the correct length.
    (assert-equal 3 (length (uniform-sample population 3)))
    (assert-equal 5 (length (uniform-sample population 5)))
    
    ;; All the values aren't the same.
    (assert-true (not (= 1 (length (remove-duplicates (uniform-sample population 50))))))
    
    ;; Check that it is WITH replacement.
    ;; If we sample the same number of elements as the source, we don't get the same set.
    (assert-false
      (equal (sort population #'<)
             (sort (uniform-sample population (length population)) #'<)))))

