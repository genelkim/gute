
(in-package :gute)

(defun clos-equality-constructor (slot-equality-alist)
  "Constructs a CLOS equality function with the given association list from
  slot to equality function. The generated function is equal if for every slot
  the equality function holds for that slot between two CLOS objects.

  Returns a lambda object of the constructed function."
  #'(lambda (obj1 obj2)
      (every
        #'(lambda (slot-equality-cons)
            (let ((slot (car slot-equality-cons))
                  (equality-fn (cdr slot-equality-cons)))
              (funcall equality-fn
                       (slot-value obj1 slot)
                       (slot-value obj2 slot))))
        slot-equality-alist)))

