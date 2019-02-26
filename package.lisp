;; Gene Louis Kim's Lisp Utilities.
;; Started ~2018-11-15

(in-package :cl-user)

(defpackage :util
  (:use :cl :cl-user)
  (:shadowing-import-from :alexandria)
  (:shadowing-import-from :cl-ppcre)
  (:shadowing-import-from :cl-mathstats)
  (:export
    ;; io.lisp
    :read-file-lines
    :read-file-lines2
    :read-all-from-stream
    :read-all-from-file
    :read-all-from-string
    :write-to-file
    :write-list-to-file
    :princln

    ;; list.lisp
    :slice
    :remove-nth
    :split-by-cond

    ;; string.lisp
    :trim
    :left-trim
    :right-trim
    :sym2str
    :list-to-string
    :levenshtein

    ;; sexpr.lisp
    :extract-category
    :tree-find
    :tree-find-if

    ;; ttt.lisp
    :hide-ttt-ops
    :unhide-ttt-ops

    ;; symbol.lisp
    :split-into-atoms
    :fuse-into-atom
    *intern-caller-pkg*
    :inout-intern
    :in-intern

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
    :punctuation-p
    :remove-punctuation

    ;; reachability-indexing.lisp
    :reachable
    :depth-label-graph

    ;; hashtable.lisp
    :print-hash-table-readably
    :print-ht

    ;; function.lisp
    :compose

    ;; stats.lisp
    :precision
    :group-precisions
    :macro-precision
    :micro-precision
    ))

