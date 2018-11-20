;; Gene Louis Kim's Lisp Utilities.
;; Started ~2018-11-15

(asdf:defsystem :util
  :depends-on (:alexandria :cl-ppcre)
  :components ((:file "package")
               (:file "io")
               (:file "list")
               (:file "string")
               (:file "sexpr")
               (:file "ttt")
               (:file "lore-util")
               (:file "lang")
               (:file "reachability-indexing")))

