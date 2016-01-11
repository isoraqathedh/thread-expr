(defpackage #:info.isoraqathedh.thread-expr.asdf
  (:use #:cl #:asdf))
(in-package #:info.isoraqathedh.thread-expr.asdf)

(defsystem thread-expr
  :name "Thread-expr macro"
  :version "0.1"
  :license "MIT"
  :components ((:file "thread-expr")))
