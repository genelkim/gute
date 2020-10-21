;; Gene Louis Kim's Lisp Utilities.

(in-package :cl-user)

(defpackage :gene-utilities
  (:nicknames :gute)
  (:use :cl :cl-user)
  (:import-from :cl-mathstats :mean)
  ;(:shadowing-import-from :alexandria)
  ;(:shadowing-import-from :cl-ppcre)
  (:export
    ;; general.lisp
    define-constant
    add-nickname
    safe-intern
    lisp-impl
    sbcl-impl?
    acl-impl?
    cmucl-impl?
    sbcl
    acl
    cmucl
    safe-symbol-eval
    argv

    ;; io.lisp
    read-file-lines
    read-file-lines2
    read-all-from-stream
    read-all-from-file
    read-all-from-string
    write-to-file
    write-list-to-file
    princln

    ;; list.lisp
    insert
    slice
    remove-nth
    split-by-cond
    interleave
    pair-up-list
    powerset
    permute
    label-with-num

    ;; string.lisp
    trim
    left-trim
    right-trim
    sym2str
    atom2str
    list-to-string
    levenshtein

    ;; sexpr.lisp
    extract-category
    tree-find
    tree-find-if

    ;; symbol.lisp
    split-into-atoms
    fuse-into-atom
    *intern-caller-pkg*
    inout-intern
    in-intern

    ;; lore-util.lisp
    get-line
    mintersection
    tree-from-string
    intern-symbols-recursive
    extract-sentence
    contains-underscore
    split-at-char
    split-at-underscore
    subst-in-symb
    memberp
    prune
    safe-car safe-first safe-second safe-third safe-third
    safe-fourth safe-fifth safe-cdr safe-cddr safe-cdddr
    sub
    symb
    mkstr
    slurp
    bind in-case-error with-outfile with-infile do-lines do-lines-slurp

    ;; lang.lisp
    vowel?
    consonant?
    add-indefinite
    indefinite-article
    plural?
    singular?
    plural-of
    singular-of
    simple-past
    past-participle
    present-singular
    apply-ing
    transform-number
    capitalize-words
    punctuation-p
    remove-punctuation
    add-contractions
    contraction-possibilities

    ;; reachability-indexing.lisp
    reachable
    depth-label-graph

    ;; hashtable.lisp
    print-hash-table-readably
    print-ht

    ;; function.lisp
    nilfn
    tfn
    compose
    grouped-arglist
    argnum
    required-argnum
    key-argnum
    optional-argnum

    ;; stats.lisp
    precision
    group-precisions
    macro-precision
    micro-precision
    cartesian-product

    ;; memo.lisp
    memoize
    memo
    clear-memoize
    defun-memo

    ;; regex.lisp
    overlap-regex-matches
    overlap-regex-matches-as-strings
    regex-alist-to-scanner-alist

    ;; queue.lisp
    queue
    make-queue
    queue-empty-p
    enqueue
    dequeue
    queue-peek

    ;; time.lisp
    timing
    ))

