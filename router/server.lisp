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

(defun start-server (server-name &key dev)
  (declare (string server-name))
  ;; complete server-info
  (setf (name *server-info*) server-name
        (port *server-info*) (config-get-server-option server-name "server-port"))
  ;; start hunchentoot server
  ;; just serving static pages in www/ directory
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :document-root #P"router/www/"
                                    :port (port *server-info*)))
  ;; define handling for ajax queries to databse (url /query)
  (hunchentoot:define-easy-handler (query :uri "/query" :default-request-type :get) (command)
    (let* ((tokens (split-query-to-tokens command))
           (action (car tokens))
           (args (cdr tokens))
           (key (car args)))
      (cond
        ((string= "help" action) (help-message))
        ;; ((string= "create" action)
        ;;  (if (not key)
        ;;      (error-message 'key-not-specified)
        ;;      (apply #'db-create args)))
        ;; ((string= "get" action)
        ;;  (if (not key)
        ;;      (error-message 'key-not-specified)
        ;;      (print-entry (db-retrieve key))))
        ;; ((string= "update" action)
        ;;  (if (not key)
        ;;      (error-message 'key-not-specified)
        ;;      (apply #'db-update args)))
        ;; ((string= "remove" action)
        ;;  (if (not key)
        ;;      (error-message 'key-not-specified)
        ;;      (db-delete key)))
        ;; ((string= "flush" action)
        ;;  (db-flush))
        (t (error-message 'unknown-command))))))

(defun help-message ()
  "Available commands:
help - show this message;
create KEY [NAME] [PHONE] - create entry in database with specified KEY;
get KEY - retrieve entry with specified KEY;
update KEY [NAME] [PHONE] - update enry information;
remove KEY - remove entry with specified key from database;
flush - save current database state to disk;

Remember - if you specify more args than needed, all extra args will be ignored")
