;; Gene Louis Kim's Lisp Utilities.
;; Started ~2018-11-15

(asdf:defsystem :util
  :depends-on (:alexandria :cl-ppcre)
  :components ((:file "package")
               (:file "list")
               (:file "io")
               (:file "string")
               (:file "sexpr")
               (:file "ttt")
               (:file "symbol")
               (:file "lore-util")
               (:file "lang")
               (:file "reachability-indexing")))

