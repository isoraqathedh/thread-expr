(defpackage :info.isoraqathedh.thread-expr
  (:use :cl)
  (:nicknames :thread-expr)
  (:export :-> :->> :with-expression-threading))

(in-package :thread-expr)

(defmacro -> (argument &body functions)
  "Threads ARGUMENT through FUNCTIONS, with ARGUMENT as the first argument.

A re-implementation of the identically-named function in Clojure."
  (reduce #'(lambda (arg fun)
              (if (listp fun)
                  (destructuring-bind (fname &rest fargs) fun
                    `(,fname ,arg ,@fargs))
                  `(,fun ,arg))) functions
                  :initial-value argument))

(defmacro ->> (argument &body functions)
  "Threads ARGUMENT through FUNCTIONS, with ARGUMENT as the last argument.

A re-implementation of the identically-named function in Clojure."
  (reduce #'(lambda (arg fun)
              (if (listp fun)
                  (destructuring-bind (fname &rest fargs) fun
                    `(,fname ,@fargs ,arg))
                  `(,fun ,arg))) functions
                  :initial-value argument))

(defmacro with-expression-threading ((argument &optional (thread-symbol :||))
                                     &body forms)
  "Threads ARGUMENT through FORMS anaphorically.

Each form in FORMS must have one or more of a symbol THREAD-SYMBOL in it.
Each instance of that symbol is replaced with the form preceding it.
As a special case, `(foo THREAD-SYMBOL)` can be written simply as `foo`.
All forms evaluated once only. Preserves multiple values.

Ideally used for expressing the flow of data through a set of functions."
  (let (final-gensym
        (last-form (car (last forms)))
        (other-forms (butlast forms)))
    (flet ((substitute-thread-symbol (gensym thread-sym form)
             (if (listp form)
                 (substitute gensym thread-sym form)
                 (list form gensym))))
      `(let* ,(loop for form in other-forms
                    for this-gensym = (gensym)
                    and prev-gensym = argument then this-gensym
                    collect (list this-gensym
                                  (substitute-thread-symbol
                                   prev-gensym thread-symbol form))
                    finally (setf final-gensym this-gensym))
         ,(substitute-thread-symbol final-gensym thread-symbol last-form)))))
