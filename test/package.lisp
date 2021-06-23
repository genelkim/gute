(defpackage #:gute/tests
  (:use #:cl #:lisp-unit #:gute)
  (:export #:run))

(in-package :gute/tests)

(defun run (&key tests tags
                 ;; lisp-unit verbosity parameters.
                 (print-failures t)
                 (print-errors t)
                 (print-summary t)
                 (summarize-results t))
  "Run all tests.
  
  Optional arguments:
    tests:  list of test names, defaults to running all tests
    tags:   list of test tags

  `tests` and `tags` should not both be set. The `tags` argument will be
  ignored if that is the case.
  "
  (let ((*print-failures* print-failures)
        (*print-errors* print-errors)
        (*print-summary* print-summary)
        (*summarize-results* summarize-results))
    ;; Run tests.
    (cond
      ;; Specified tests.
      (tests
        (when tags
          (warn (concatenate 'string
                             "Both the :tags and :tests keyword "
                             "arguments are given for gute/tests:run. "
                             "Ignoring the :tags argument...")))
        (in-intern (tests pkgtests :gute/tests)
          (lisp-unit:run-tests pkgtests :gute/tests)))
      ;; Specified tags.
      (tags (run-tags tags :gute/tests))
      ;; Default, all tests.
      (t (run-tests :all :gute/tests)))))

