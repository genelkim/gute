(asdf:defsystem :gute
  :name "Gene's Utilities"
  :serial t
  :version "1.0.2"
  :description "Gene's personal kitchen sink library."
  :author "Gene Louis Kim <gkim21@cs.rochester.edu>"
  :license "MIT"
  :depends-on (:alexandria :bordeaux-threads (:version #:cl-ppcre "2.0.4")
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
               (:file "time")
               (:file "random"))
  :around-compile (lambda (next)
                    ; For development use (debug 3) (safety 3) (space 1) (speed 1)
                    (proclaim '(optimize (debug 0) (safety 2) (space 1) (speed 3)))
                    (funcall next))
  :in-order-to ((test-op (test-op :gute/tests))))

(asdf:defsystem :gute/tests
  :serial t
  :description "Tests for the GUTE library"
  :author "Gene Louis Kim <gkim21@cs.rochester.edu>"
  :license "MIT"
  :depends-on (:gute :lisp-unit)
  :components ((:file "test/package")
               (:file "test/random"))
  :perform (test-op (o c) (symbol-call :gute/tests :run)))

