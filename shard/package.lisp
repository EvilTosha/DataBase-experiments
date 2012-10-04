(in-package :cl-user)

(defpackage :shard
  (:use :cl :hunchentoot :cl-fad :split-sequence :yason #:trivial-http)
  (:export #:db-load #:config-load))
