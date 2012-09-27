;;;; package.lisp

(defpackage #:m-util
  (:use #:cl)
  (:export
    ;; Gensym stuff
    #:with-gensyms

    ;; Other generic stuff
    #:nil!
    #:t!

    #:with-hash-table-iterator*
    #:hash-table-keys-to-list

    #:flatten
    #:now

    ;; The dlambda
    #:dlambda

    ;; Anaphoras
    #:it
    #:self
    #:self-clause
    #:test
    #:alambda
    #:aif
    #:aif-nonzero
    #:aif-zero
    #:awhen
    #:aunless
    #:alet
    #:adlambda))

