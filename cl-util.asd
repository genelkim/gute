;; Gene Louis Kim's Lisp Utilities.
;; Started ~2018-11-15

(asdf:defsystem :cl-util
  :depends-on (:alexandria (:version #:cl-ppcre "2.0.4")
               :cl-mathstats :cl-strings :conium)
  :components ((:file "package")
               (:file "general")
               (:file "list")
               (:file "io")
               (:file "string")
               (:file "sexpr")
               (:file "symbol")
               (:file "lore-util")
               (:file "regex")
               (:file "lang")
               (:file "reachability-indexing")
               (:file "hashtable")
               (:file "function")
               (:file "stats")
               (:file "memo")
               (:file "queue")
               (:file "time")))

