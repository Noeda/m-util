;;;; anaphora.lisp
;; Anaphoric macros
;;

(in-package #:m-util)

(defmacro aif (test true-clause &optional false-clause)
  `(let ((it ,test))
     (if it
       ,true-clause
       ,false-clause)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it ,@body)))

(defmacro aunless (test &body body)
  `(let ((it ,test))
     (unless it ,@body)))

(defmacro aif-nonzero (test true-clause &optional false-clause)
  `(let ((it ,test))
     (if (= it 0)
       ,false-clause
       ,true-clause)))

(defmacro aif-zero (test true-clause &optional false-clause)
  `(aif-nonzero ,test ,false-clause ,true-clause))

(defmacro self-bind-lambda (name args &body body)
  `(labels ((,name ,args
              ,@body))
     (function ,name)))

(defmacro alambda (args &body body)
  `(self-bind-lambda self ,args ,@body))

(defmacro alet (letargs &body body)
  `(let ((this) ,@letargs)
     (setf this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defmacro adlambda (&rest bodies)
  (with-gensyms (dispatch-sym args-sym)
    `(alambda (,dispatch-sym &rest ,args-sym)
       (case ,dispatch-sym
         ,@(mapcar (lambda (x)
                     (let ((first-sym (first x))
                           (args (second x)))
                       `(,(if (consp first-sym)
                            first-sym
                            (list first-sym))
                          (apply (self-bind-lambda self-clause ,args
                                   ,@(cddr x))
                                 ,args-sym))))
                   bodies)
         (otherwise
           (error "No matching clause to dlambda; got ~a" ,dispatch-sym))))))


