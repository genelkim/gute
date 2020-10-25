
(in-package :gute)

(defun read-file-lines (filename)
  "Reads a file line by line and return a list of strings."
  (labels
    ((helper (in acc)
       (multiple-value-bind (line endp)
           (read-line in nil)
         (if endp
           (cons line acc)
           (helper in (cons line acc))))))
    (with-open-file (stream filename)
      (reverse (remove-if #'null (helper stream '()))))))

(defun read-file-lines2 (filename)
  "Reads a file line by line and return a list of strings.
  Done in a loop so you won't get a stack overflow even with bad compiler
  parameters."
  (with-open-file (fh filename)
    (let ((done nil)
          (acc '()))
      (loop while (not done)
            do (multiple-value-bind (line newdone)
                                    (read-line fh nil)
                 (setf done newdone)
                 (push line acc)))
      (reverse acc))))

; NOT FUNCTONAL: Two consecutive unread-char operations are not permitted.
(defun read-block-comment (s)
  "Reads off a block comment (#| ... |#) from the stream. If the stream does
  not start with #|, nothing occurs. Return value is undefined. Raises and
  eof-error if the stream ends without completion of the block comment."
  (let (char1 char2 (nestlvl 1))
    (if (and (eql #\# (setf char1 (read-char s nil 'eof)))
             (eql #\| (setf char2 (read-char s nil 'eof))))
      (do ((char1 char2 char2)
           (char2 (read-char s) (read-char s)))
          ;; End if we are about to decrement to nesting level 0. This
          ;; pre-empting avoids an eof-error when the stream ends exactly as
          ;; the block comment ends.
          ((and (= 1 nestlvl) (eql #\| char1) (eql #\# char2)))
        (cond
          ((and (eql #\# char1) (eql #\| char2)) (incf nestlvl))
          ((and (eql #\| char1) (eql #\# char2)) (decf nestlvl))))
      ;; Restore any read characters in the test.
      (progn
        (when char2 (unread-char char2 s))
        (when char1 (unread-char char1 s))))))

(defun read-all-from-stream (s &optional (placeholder-base 'gute-block-comment))
  "Reads all s-expressions from a character stream until exhausted. It will
  raise an eof-error if the stream does not represent a sequence of
  s-expresssions. Comments are ignored.

  The handling of file-ending block comments are done by redefining the block
  comment dispatch function to return the concatenation of a generated symbol
  and a specific base symbol. This is only checked when we come across a #\#
  macro dispatch, so the method will fail if some other #\# dispatch macro
  returns this symbol. This is extremely unlikely, but not impossible.

  The optional argument placeholder-base allows the caller to specify a
  placeholder that is known to not conflict with any read result."
  (let ((prior-block-comment-dispatch
          (get-dispatch-macro-character #\# #\|))
        ;; Generate placeholder constant.
        (placeholder-const (fuse-into-atom (list (gensym) placeholder-base)))
        result)
    ;; Set our custom block comment dispatch function.
    ;; Does exactly what we'd do before, but returns a placeholder symbol.
    (set-dispatch-macro-character #\# #\|
      #'(lambda (s c n)
          (funcall prior-block-comment-dispatch s c n)
          placeholder-const))
    (labels
      ((helper (in acc)
         ;; Read off all comments and whitespace.
         (loop while (eql #\; (peek-char t in nil))
               do (read-line in nil))
         (if (not (peek-char t in nil))
           ;; End of file---NB: neither () or nil are single-characters.
           acc
           ;; Recurse.
           (let ((first-char (peek-char t in nil))
                 (expr (read in))
                 (newacc))
             ;; If the first character is # check whether the read expression is
             ;; a block comment via the locally defined reader result. If so,
             ;; ignore.
             (setf newacc (if (and (eql first-char #\#)
                                   (equal expr placeholder-const))
                            acc
                            (cons expr acc)))
             (helper in newacc)))))
      (setf result (reverse (helper s nil)))
      ;; Reinstate old dispatch function.
      (set-dispatch-macro-character #\# #\| prior-block-comment-dispatch)
      ;; Return result
      result)))

(defun read-all-from-file (filename)
  "Reads all s-expressions from the given file until exhausted.
  It will raise an error if the file does not contain a sequence of valid
  s-expresssions."
  (with-open-file (s filename)
    (read-all-from-stream s)))

;; TODO: move this to string-ops.lisp or maybe stream.lisp once this directory
;; is packaged and internal dependencies are better managed.
(defun read-all-from-string (str)
  "Reads all s-expressions from the given string.
  Raises an error if the string does not represent a series of valid
  s-expressions. Same as READ-ALL-FROM-FILE, but for strings."
  (with-input-from-string (s str)
    (read-all-from-stream s)))

(declaim (inline write-to-file))
(defun write-to-file (str filename)
  "Writes a string to a file."
  (declare (optimize (speed 1)))
  (declare (type simple-string str))
  (with-open-file (fh filename :direction :output)
    (format fh str)))

(defun write-list-to-file (lst filename &optional (sep "~%"))
  "Writes a list to a file.
  Depends on write-to-file."
  (write-to-file (list-to-string lst sep) filename))

(defun princln (x)
  "CL version of 'println' in Java.
  The name PRINCLN is meant to reflect the CL naming conventions for prints."
  (princ x)
  (format t "~%"))

