;; Gene Louis Kim's Lisp Utilities.
;; Started ~2018-11-15

(in-package :cl-user)

(defpackage :util
  (:use :cl :alexandria :cl-ppcre)
  (:export
    ;; io.lisp
    :read-file-lines
    :read-all-from-stream
    :read-all-from-file
    :read-all-from-string

    ;; list.lisp
    :slice
    :remove-nth
    :split-by-cond

    ;; string.lisp
    :trim
    :left-trim
    :right-trim

    ;; sexpr.lisp
    :extract-category

    ;; ttt.lisp
    :hide-ttt-ops
    :unhide-ttt-ops

    ;; lore-util.lisp
    :get-line
    :mintersection
    :tree-from-string
    :intern-symbols-recursive
    :extract-sentence
    :contains-underscore
    :split-at-char
    :split-at-underscore
    :subst-in-symb
    :memberp
    :prune
    :safe-car :safe-first :safe-second :safe-third :safe-third
    :safe-fourth :safe-fifth :safe-cdr :safe-cddr :safe-cdddr
    :sub
    :symb
    :mkstr
    :slurp
    :bind :in-case-error :with-outfile :with-infile :do-lines :do-lines-slurp

    ;; lang.lisp
    :vowel?
    :consonant?
    :add-indefinite
    :indefinite-article
    :plural?
    :singular?
    :plural-of
    :singular-of
    :simple-past
    :past-participle
    :present-singular
    :apply-ing
    :transform-number
    :capitalize-words

    ;; reachability-indexing.lisp
    :reachable
    :depth-label-graph
    ))

