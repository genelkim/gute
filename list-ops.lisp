;; Returns a slice of the list with given starting and ending indices,
;; inclusive and exclusive, respectively.
(defun slice (lst start end)
  (labels
    ((helper
       (cur index acc)
       (cond
         ;; Ran out of list.
         ((null cur) acc)
         ;; Index past end.
         ((>= index end) acc)
         ;; Recursive case, in range.
         ((and (>= index start)
               (< index end))
          (helper (cdr cur) (+ index 1) (cons (car cur) acc)))
         ;; Recursive case before start.
         (t (helper (cdr cur) (+ index 1) acc)))))
    (reverse (helper lst 0 '()))))

;; Returns a list without the nth element.
(defun remove-nth (n lst)
  (append (subseq lst 0 n) (nthcdr (1+ n) lst)))

;; Returns a list of lst with cndn filter out followed by lst with only cndn.
(defun split-by-cond (lst cndn)
  (list (remove-if cndn lst)
        (remove-if-not cndn lst)))

