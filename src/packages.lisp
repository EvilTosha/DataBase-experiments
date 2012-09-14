(in-package :cl-user)

(defpackage :database
  (:nicknames #:db)
  (:use :cl :hunchentoot :cl-fad :split-sequence :yason)
  (:export #:load-db))
