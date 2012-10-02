;;;; m-util.lisp

(in-package #:m-util)

(defmacro with-gensyms (syms &body body)
  "Binds a unique symbol (through GENSYM) to each symbol listed
  in SYMS and then executes BODY with those bindings."
  `(let (,@(mapcar (lambda (s)
                     `(,s (gensym)))
                   syms))
     ,@body))

(defmacro nil! (place)
  "Given a place, sets it to NIL."
  `(setf ,place nil))

(defmacro t! (place)
  "Given a place, sets it to T."
  `(setf ,place t))

(defmacro dlambda (&rest bodies)
  "Given bodies, returns a function that can be called to select
  one of the bodies to be executed.

  Each body should be a list that starts with a symbol. The element
  after the symbol should be a lambda list. The body forms to be
  evaluated come after that.

  The body chosen depends on the first argument given to the dlambda.

  Here's an example of a counter:

  (defparameter *counter* (let ((i 0))
                            (dlambda
                              (:add (n) (setf i (+ i n)))
                              (:dec (n) (setf i (- i n)))
                              (:val () i))))

  (funcall *counter* :add 17) => 17
  (funcall *counter* :val) => 17
  (funcall *counter* :dec 19) => -2
  (funcall *counter* :val) => -2
  "
  (with-gensyms (dispatch-sym args-sym)
    `(lambda (,dispatch-sym &rest ,args-sym)
       (ecase ,dispatch-sym
         ,@(mapcar (lambda (x)
                     (let ((first-sym (first x))
                           (args (second x)))
                       `(,(if (consp first-sym)
                            first-sym
                            (list first-sym))
                          (apply (lambda ,args
                                   ,@(cddr x))
                                 ,args-sym))))
                   bodies)))))

(defmacro mvb (&rest rest)
  "Shorthand for MULTIPLE-VALUE-BIND"
  `(multiple-value-bind ,@rest))

(defmacro dbind (&rest rest)
  "Shorthand for DESTRUCTURING-BIND"
  `(destructuring-bind ,@rest))

(defmacro with-hash-table-iterator* ((key-var value-var hash-table)
                                     &body body)
  "Iterates over a hash table. At each iteration, the variable pointed by
KEY-VARIABLE is bound to the key of a hash table pair and VALUE-VAR is
bound to the value respectively. You can escape the iteration with
((RETURN)."
  (with-gensyms (generator-sym more?-sym)
    `(with-hash-table-iterator (,generator-sym ,hash-table)
       (loop
         (multiple-value-bind (,more?-sym ,key-var ,value-var)
             (,generator-sym)
           (declare (ignorable ,key-var ,value-var))
           (unless ,more?-sym (return))
           ,@body)))))

(defun hash-table-keys-to-list (hash-table)
  "Given a hash table, returns all the keys in it as a fresh list."
  (let ((results))
    (with-hash-table-iterator* (key value hash-table)
      (push key results))
    results))

(defun flatten (structure)
  "Given a list, returns a flattened list. That is, no sublists. It also
  turns atoms into lists with single element."
  (cond ((null structure) nil)
        ((atom structure) `(,structure))
        (t (mapcan #'flatten structure))))

(defun now ()
  "Returns the current time in seconds using floating point value."
  (/ (coerce (get-internal-real-time) 'float)
     (coerce internal-time-units-per-second 'float)))

(defun unique (&optional name)
  "Returns a unique value each time it is called. The actual type of the
  value, at the moment, is a simple cons with random data. Each unique cons
  is not EQUAL to any other cons. NAME can be given. It is put inside the
  cons. When the cons is printed, the name can be seen. Two uniques with
  the same name are still unique, that is to say, NAME is only for cosmetic
  purposes."
  (cons (gensym) name))

(defmacro unwind-protect-if-fails (test &body cleanups)
  "Same as UNWIND-PROTECT but only executes cleanup forms if TEST does not
  exit normally."
  (with-gensyms (do-cleanup)
    `(let ((,do-cleanup t))
       (unwind-protect
           (multiple-value-prog1
             ,test
             (setq ,do-cleanup nil))
         (when ,do-cleanup
           ,@cleanups)))))

