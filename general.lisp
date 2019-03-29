
(in-package :util)

;; Robust version of defconstant that is faithful to the strict ANSI
;; definition.
(defmacro define-constant (name value &optional doc)
	`(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
								,@(when doc (list doc))))

(defun add-nickname (package nickname)
  (when (stringp package)
    (setf package (find-package package)))
  (check-type package package)
  (check-type nickname string)
  (rename-package package (package-name package)
                  (adjoin nickname (package-nicknames package)
                          :test #'string=)))

;; Safe intern.
;;  x can be a string or a symbol and it interns it.
(defun safe-intern (strsym &optional pkg)
  (cond
    ((stringp strsym) (intern strsym pkg))
    ((symbolp strsym) (intern (symbol-name strsym) pkg))
    (t (error "The input to safe-intern is not a supported data type."))))

;;
;; Functions for determining lisp implementation.
;;

(defvar *lisp-implementation-to-shorthand*
  '(("SBCL" . sbcl)
    ("International Allegro CL Free Express Edition" . acl)
    ("International Allegro CL Enterprise Edition" . acl)
    ("CMU Common Lisp" . cmucl)))
;; Returns a symbol of the lisp implementation.
;; This uses the implementation shorthands rather than the idiosyncratic names
;; returned from #'cl:lisp-implementation-type
(defun lisp-impl ()
  (let ((impl-str (lisp-implementation-type)))
    (cdr (assoc impl-str *lisp-implementation-to-shorthand* :test #'string=))))
;; Functions for specific implementations.
(defun sbcl-impl? ()
  (eql (lisp-impl) 'sbcl))
(defun acl-impl? ()
  (eql (lisp-impl) 'acl))
(defun cmucl-impl? ()
  (eql (lisp-impl) 'cmucl))

