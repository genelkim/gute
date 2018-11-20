;; Utility functions that build on top of the tree-to-tree transduction library
;; (TTT).

(in-package :util)

(defun hide-ttt-ops (wff); tested
;~~~~~~~~~~~~~~~~~~~~~~~~
; Wrap [..] around symbols like !, +, ?, *, @, ~, {}, or <>, or
; ones starting this way, which we may want to use in some patterns
; (e.g., in wff-patterns involving *, **, @, or ~), but can't 
; because of their special meanings in TTT. We're assuming that
; the wffs we want to process don't *already* contain symbols in
; square brackets, starting as above inside the brackets, and which
; shouldn't have the brackets removed when we ultimately "unhide"
; the hidden symbols in a formula.
;
  (let (str chars)
       (cond ((symbolp wff)
              (setq str (string wff))
              (setq chars (coerce str 'list))
              (cond ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~))
                     (intern (concatenate 'string "[" str "]")))
                    ((and (eq (car chars) #\{) (eq (second chars) #\}))
                     (intern (concatenate 'string "[" str "]")))
                    ((and (eq (car chars) #\<) (eq (second chars) #\>))
                     (intern (concatenate 'string "[" str "]")))
                    (t wff)))
             ((atom wff) wff)
             (t (cons (hide-ttt-ops (car wff)) (hide-ttt-ops (cdr wff)))))
 )); end of hide-ttt-ops


(defun unhide-ttt-ops (wff); tested
;~~~~~~~~~~~~~~~~~~~~~~~~~~
; Remove the square brackets that have been added around ttt symbols
; in wff by 'hide-ttt-ops':
;
 (let (str chars)
      (cond ((symbolp wff)
             (setq str (string wff))
             (setq chars (coerce str 'list))
             (cond ((or (not (eq (car chars) #\[))
                        (not (eq (car (last chars)) #\]))) wff)
                   (t (setq chars (cdr (butlast chars)))
                      (setq str (coerce chars 'string))
                      (cond ((null chars) wff)
                            ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~))
                             (intern str))
                            ((and (eq (car chars) #\{) (eq (second chars) #\}))
                             (intern str))
                            ((and (eq (car chars) #\<) (eq (second chars) #\>))
                             (intern str))
                            (t wff)))))
            ((atom wff) wff)
            (t (cons (unhide-ttt-ops (car wff)) (unhide-ttt-ops (cdr wff)))))
 )); end of unhide-ttt-ops

