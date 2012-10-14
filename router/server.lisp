;;;; server.lisp

;;;; This file contains code required for setting up server and terminal for
;;;; performing simple ajax interface to database

(in-package :router)

(defclass server-info ()
  ((name         :initarg :name         :initform ""                    :accessor name)
   (port         :initarg :port         :initform 0                     :accessor port)
   (type         :initarg :type         :initform "router")
   (start-time   :initarg :start-time   :initform (get-universal-time)  :accessor start-time)))

(defvar *server-info* (make-instance 'server-info))

(defun start-server (server-name)
  (declare (string server-name))
  ;; complete server-info
  (setf (name *server-info*) server-name
        (port *server-info*) (config-get-server-option server-name "server-port")
        ;; init buckets list for hashing
        *bucket-ends* (config-get-buckets))
  ;; start hunchentoot server
  ;; just serving static pages in www/ directory
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :document-root #P"router/www/"
                                    :port (port *server-info*)))
  ;; define handling for ajax queries to databse (url /query)
  (hunchentoot:define-easy-handler (query :uri "/query" :default-request-type :get) (command)
    (let* ((tokens (split-query-to-tokens command))
           (action (car tokens))
           (args (cdr tokens)))
      ;; some magic because of huchentoot's strange behavior (dont accept multiple returned values)
      (nth-value 0 (handle-db-request action args)))))
