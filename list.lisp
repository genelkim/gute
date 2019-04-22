
(in-package :util)


;; Inserts x to a list lst at position i.
;; Destructive.
(defun insert (x lst i)
  (if (= i 0)
    (push x lst)
    (push x (cdr (nthcdr (1- i) lst))))
  lst)


;; Returns a slice of the list with given starting and ending indices,
;; inclusive and exclusive, respectively.
(defun slice (lst start end)
  (declare (type fixnum start end)
           (type list lst))
  (labels
    ((helper
       (cur index acc)
       (declare (type fixnum index)
                (type list cur acc))
       (cond
         ;; Ran out of list.
         ((null cur) acc)
         ;; Index past end.
         ((>= index end) acc)
         ;; Recursive case, in range.
         ((and (>= index start)
               (< index end))
          (helper (cdr cur) (1+ index) (cons (car cur) acc)))
         ;; Recursive case before start.
         (t (helper (cdr cur) (1+ index) acc)))))
    (reverse (helper lst 0 '()))))

;; Returns a list without the nth element.
(defun remove-nth (n lst)
  (declare (type fixnum n)
           (type list lst))
  (append (subseq lst 0 n) (nthcdr (1+ n) lst)))

;; Returns a list of lst with cndn filter out followed by lst with only cndn.
(defun split-by-cond (lst cndn)
  (declare (type list lst))
  (list (remove-if cndn lst)
        (remove-if-not cndn lst)))

;; Returns a list where the items alternate between the items of lst1 and lst2.
(defun interleave (lst1 lst2)
  (labels
    ;; Helper function, builds the interleaving in reverse.
    ;; Reduces the number of base and recursive cases by swapping l1 and l2
    ;; between recursive calls.
    ((helper (l1 l2 acc)
       (cond
         ((null l1) (append (reverse l2) acc))
         (t (helper l2 (cdr l1) (cons (car l1) acc))))))
    (reverse (helper lst1 lst2 nil))))

;; (a b c d) -> ((a b) (c d))
;; Assumes that the list is of even length and doesn't contain nil elements.
(defun pair-up-list (lst)
  (reverse (car (reduce
                  #'(lambda (acc cur)
                      (if (cdr acc)
                        (cons (cons (list (cdr acc) cur) (car acc))
                              nil)
                        (cons (car acc) cur)))
                  lst
                  :initial-value '(nil . nil)))))

;; From https://rosettacode.org/wiki/Power_set#Common_Lisp
(defun powerset (s)
  (if s (mapcan (lambda (x) (list (cons (car s) x) x))
                (powerset (cdr s)))
      '(())))

;; From https://rosettacode.org/wiki/Permutations#Common_Lisp
(defun permute (list)
  (if list
    (mapcan #'(lambda (x)
		(mapcar #'(lambda (y) (cons x y))
			(permute (remove x list))))
	    list)
    '(()))) ; else

