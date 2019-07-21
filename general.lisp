
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
  (if (eq pkg (find-package "COMMON-LISP"))
    (setf pkg *package*))
  (cond
    ((stringp strsym) (intern strsym pkg))
    ((symbolp strsym) (intern (symbol-name strsym) pkg))
    ((numberp strsym) strsym)
    (t (error
         (format nil
                 "The input to safe-intern is not a supported data type.~%Value: ~s~%Type: ~s~%"
                 strsym (type-of strsym))))))

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

;; Evaluates a symbol with respect to the given package iff the package is
;; available and the symbol is found in that package.
(defun safe-symbol-eval (sym pkg-name)
  (let ((pkg (find-package pkg-name)))
    (if pkg (eval (find-symbol (symbol-name sym) pkg)))))

;; Gives the argv depending on the distribution.
(defun argv ()
  (or
    #+SBCL sb-ext:*posix-argv*
    #+LISPWORKS system:*line-arguments-list*
    #+CMU extensions:*command-line-words*
    #+ALLEGRO (sys:command-line-arguments)
    #+CLISP *args*
    nil))

