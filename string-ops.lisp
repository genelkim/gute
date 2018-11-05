
;; List of characters that constitute whitespace in the trim functions.
(defparameter *trim-whitespace-chars*
  '(#\Space #\Newline #\Backspace #\Tab
    #\Linefeed #\Page #\Return #\Rubout))

;; Trims a string of whitespace.  Optionally you can provide the character list
;; that are trimmed, though in that case you might as well use string-trim.
(defun trim (str &optional trimlist-arg)
  (let ((trimlist (if trimlist-arg trimlist-arg
                    *trim-whitespace-chars* )))
  (string-trim trimlist str)))

;; Same as trim, but only for the left side.
(defun left-trim (str &optional trimlist-arg)
  (let ((trimlist (if trimlist-arg trimlist-arg
                    *trim-whitespace-chars*)))
  (string-left-trim trimlist str)))

;; Same as trim, but only for the right side.
(defun right-trim (str &optional trimlist-arg)
  (let ((trimlist (if trimlist-arg trimlist-arg
                    *trim-whitespace-chars*)))
  (string-right-trim trimlist str)))

;; Reads all s-expressions from the given string.  Raises an error if the
;; string does not represent a series of valid s-expressions.
;; This corresponds to 'read-file-objects' in file-io.lisp, but for strings.
;; TODO: make names consistent between the two and implement one in terms of
;; the other 
(defun read-all-from-string (topstr)
  (labels
    ;; Helper function doing all the heavy lifting.
    ((helper (str acc)
       (multiple-value-bind (sexpr endidx) (read-from-string str)
         (cond
           ;; We're at the end, so return.
           ((>= endidx (length str))
            (reverse (cons sexpr acc)))
           ;; Otherwise, recurse.
           (t (helper (subseq str endidx) (cons sexpr acc)))))))
    (helper topstr nil)))

