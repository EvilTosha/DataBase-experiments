(in-package :cl-user)

(defpackage :router
  (:use :cl :hunchentoot :cl-fad :split-sequence :yason #:trivial-http)
  (:export #:config-load #:start-server))
