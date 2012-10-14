;;;; server.lisp

;;;; This file contains code required for setting up server and terminal for
;;;; performing simple ajax interface to database

(in-package :shard)

(defclass server-info ()
  ((name              :initarg :name             :initform ""                    :accessor name)
   (port              :initarg :port             :initform 0                     :accessor port)
   (type              :initarg :type             :initform "shard")
   (master-specifier  :initarg :master-specifier :initform nil                   :accessor master-p)
   (start-time        :initarg :start-time       :initform (get-universal-time)  :accessor start-time)
   (last-update-time  :initarg :last-update-time :initform (get-universal-time)  :accessor last-update-time)))

(defvar *server-info* (make-instance 'server-info))

(defun start-server (server-name master-specifier &key dev)
  (declare (string server-name) (boolean master-specifier))
  ;;complete server info
  (setf (name *server-info*) server-name
        (port *server-info*) (config-get-server-option server-name "server-port" master-specifier)
        (master-p *server-info*) master-specifier)
  ;; start hunchentoot server
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :port (port *server-info*)))
  ;; print simple server information at root uri
  (hunchentoot:define-easy-handler (main :uri "/" :default-request-type :get) ()
    (print-simple-server-info))

  ;; define handling for ajax queries to databse (url /query)
  (hunchentoot:define-easy-handler (query :uri "/query") ((request-json :real-name "request"))
    (handle-db-request request-json)))

(defun print-simple-server-info ()
  (format nil "Shard server for simple database, source code: https://github.com/EvilTosha/DataBase-experiments <br />
Server name: ~A <br /> Master?: ~:[slave~;master~] <br /> Server port: ~D"
          (name *server-info*) (master-p *server-info*) (port *server-info*)))
