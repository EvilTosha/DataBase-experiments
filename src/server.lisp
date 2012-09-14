;;;; server.lisp

;;;; This file contains code required for setting up server and terminal for
;;;; performing simple ajax interface to database

(in-package :database)

;; load hunchentoot the webserver
(asdf:load-system :hunchentoot)

;; start hunchentoot server
;; just serving static pages in www/ directory
(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :document-root #P"www/" :port 8080))

;; define handling for ajax queries to databse (url /query)
(hunchentoot:define-easy-handler (query :uri "/query" :default-request-type :get) (command)
	;; (setf (hunchentoot:content-type*) "text/plain")
	(let* ((tokens (split-query-to-tokens command))
				 (action (car tokens))
				 (args (cdr tokens))
				 (key (car args)))
		;; check number of args
		(if (not key)
				"Error: key must be specified"
				;; else
				(cond
					((string= "help" action) (help-message))
					((string= "create" action) (apply #'db-create args))
					((string= "get" action) (db-retrieve key))
					((string= "update" action) (apply #'db-update args))
					((string= "remove" action) (db-delete key))
					(t "Unknown command, try to type 'help'")))))


(defun help-message ()
	"Available commands:
help - show this message;
create KEY [NAME] [PHONE] - create entry in database with specified KEY;
get KEY - retrieve entry with specified KEY;
update KEY [NAME] [PHONE] - update enry information;
remove KEY - remove entry with specified key from database;
flush - save current database state to disk;
halt - stop database (all non-saved information will be lost!).

Remember - if you specify more args than needed, all extra args will be ignored")
