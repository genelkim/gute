
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

