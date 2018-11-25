
(in-package :util)

(defun split-into-atoms (atm); tested
;````````````````````````````
; Useful for applying TTT to processing symbols as list of separate
; symbolic characters. Caveat: Watch out for {!,~,^,*,+,?} as part of 'atm'
; as they are specially interpreted by TTT.
 (mapcar #'intern 
    (mapcar #'(lambda (ch) (format nil "~a" ch)) 
              (coerce (string atm) 'list))))

(defun fuse-into-atom (atm-list); tested
;``````````````````````````````
; Make a single atom out of the list of atoms
; e.g., (fuse-into-atom '(this - and - that)) --> THIS-AND-THAT
 (intern (apply #'concatenate 'string (mapcar #'string atm-list))))

