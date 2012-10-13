;;;; route.lisp

;;;; File contains functions for handling http requests to database

(in-package :shard)

(defun create-json-response (error-flag error-msg data)
  "Forms response to router in JSON format"
  (declare (boolean error-flag) ((or null string) error-msg data))
  (with-output-to-string (*standard-output*)
    (json:encode-plist (list :error error-flag
                             :error-msg (or error-msg "")
                             :data (or data "")))))

(defun handle-db-request (action args)
  "Handle request from router, return json-encoded response"
  (let (;; first arg isn't always key, but for now its true
        (key (car args))
        ;; parameters for response
        error-flag
        error-msg
        data)
    (handler-case
        (setf data
              (cond
                ((string= "create" action)
                 (if (not key)
                     (error (error-message 'key-not-specified))
                     (apply #'db-create args)))
                ((string= "get" action)
                 (if (not key)
                     (error (error-message 'key-not-specified))
                     (print-entry (db-retrieve key))))
                ((string= "update" action)
                 (if (not key)
                     (error (error-message 'key-not-specified))
                     (apply #'db-update args)))
                ((string= "remove" action)
                 (if (not key)
                     (error (error-message 'key-not-specified))
                     (db-delete key)))
                ((string= "flush" action)
                 (db-flush))
                (t (error (error-message 'unknown-command)))))
      ;; handle occured error
      (error (e)
        (setf error-flag t
              error-msg (simple-condition-format-control e))))
    (create-json-response error-flag error-msg data)))
